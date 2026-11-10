#include "formal/M5Artifacts.h"

#include "formal/SMTLibWriter.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/SHA256.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <limits>
#include <map>
#include <system_error>

namespace formal {
namespace {

std::string hashBody(const std::string &Body) {
  llvm::SHA256 Hash;
  Hash.update(llvm::StringRef(Body));
  return llvm::toHex(Hash.final(), true);
}

std::string publish(const std::filesystem::path &Path,
                    const std::string &Body) {
  llvm::SmallString<256> Model(Path.string() + ".%%%%%%.tmp");
  llvm::SmallString<256> Temporary;
  int Descriptor = -1;
  if (std::error_code Error =
          llvm::sys::fs::createUniqueFile(Model, Descriptor, Temporary))
    return "cannot create temporary artifact: " + Error.message();
  {
    llvm::raw_fd_ostream File(Descriptor, true);
    File << Body;
    File.flush();
    if (File.has_error()) {
      std::error_code Error = File.error();
      llvm::sys::fs::remove(Temporary);
      return "cannot write " + Temporary.str().str() + ": " + Error.message();
    }
  }
  std::error_code Error = llvm::sys::fs::rename(Temporary, Path.string());
  if (Error)
    llvm::sys::fs::remove(Temporary);
  if (Error)
    return "cannot publish " + Path.string() + ": " + Error.message();
  return {};
}

llvm::json::Object locationJSON(const IRLocation &Location) {
  return llvm::json::Object{
      {"function", Location.Function},
      {"block", Location.BasicBlock},
      {"opcode", Location.Opcode},
      {"instruction_ordinal",
       static_cast<std::int64_t>(Location.InstructionOrdinal)},
      {"file", Location.SourceFile},
      {"line", static_cast<std::int64_t>(Location.SourceLine)},
      {"column", static_cast<std::int64_t>(Location.SourceColumn)}};
}

llvm::json::Value unsignedJSON(std::uint64_t Value) {
  if (Value <= static_cast<std::uint64_t>(
                   std::numeric_limits<std::int64_t>::max()))
    return static_cast<std::int64_t>(Value);
  return std::to_string(Value);
}

llvm::json::Object loopJSON(const LoopDiagnostic &Loop) {
  return llvm::json::Object{
      {"frame_id", unsignedJSON(Loop.FrameId)},
      {"activation_id", unsignedJSON(Loop.ActivationId)},
      {"static_loop_id", Loop.StaticLoopId},
      {"function", Loop.Function},
      {"header", Loop.Header},
      {"bound_source", Loop.BoundSource},
      {"backedge_bound", unsignedJSON(Loop.Bound)},
      {"traversed_backedges", unsignedJSON(Loop.TraversedBackedges)},
      {"attempted_backedge", unsignedJSON(Loop.AttemptedBackedge)},
      {"bound_location", locationJSON(Loop.BoundLocation)},
      {"suppressed_edge",
       Loop.SuppressedTo.empty()
           ? llvm::json::Value(nullptr)
           : llvm::json::Value(llvm::json::Object{
                 {"from", Loop.SuppressedFrom},
                 {"to", Loop.SuppressedTo}})}};
}

llvm::json::Object eventJSON(const ExecutionEvent &Event) {
  return llvm::json::Object{
      {"kind", Event.Kind},
      {"model", Event.Model},
      {"frame_id", static_cast<std::int64_t>(Event.FrameId)},
      {"object_id", static_cast<std::int64_t>(Event.ObjectId)},
      {"previous_object_id",
       static_cast<std::int64_t>(Event.PreviousObjectId)},
      {"size", static_cast<std::int64_t>(Event.Size)},
      {"source", locationJSON(Event.Location)},
      {"detail", Event.Detail}};
}

llvm::json::Object frameJSON(const CallFrameInfo &Frame) {
  return llvm::json::Object{
      {"frame_id", static_cast<std::int64_t>(Frame.FrameId)},
      {"function", Frame.Function},
      {"call_site", locationJSON(Frame.CallSite)},
      {"continuation", locationJSON(Frame.Continuation)}};
}

std::string expressionSMT(const ExprRef &Expression) {
  if (!Expression)
    return {};
  try {
    return SMTLibWriter::write(Expression, "guard", false);
  } catch (const std::exception &) {
    return "UNAVAILABLE";
  }
}

llvm::json::Object regionJSON(const PathResult::RegisteredRegion &Region,
                              std::size_t Ordinal) {
  llvm::json::Array Symbols;
  for (std::uint64_t Byte = 0; Byte < Region.Size; ++Byte)
    Symbols.push_back(std::string(Region.Input ? "input." : "output.") +
                      Region.Name + "." + std::to_string(Byte));
  llvm::json::Array Guards;
  for (const ExprRef &Guard : Region.RegistrationGuards)
    Guards.push_back(expressionSMT(Guard));
  return llvm::json::Object{
      {"kind", Region.Input ? "input" : "output"},
      {"name", Region.Name},
      {"size", static_cast<std::int64_t>(Region.Size)},
      {"ordinal", static_cast<std::int64_t>(Ordinal)},
      {"object_id", static_cast<std::int64_t>(Region.ObjectId)},
      {"offset", static_cast<std::int64_t>(Region.Offset)},
      {"source", locationJSON(Region.RegistrationLocation)},
      {"path_guards_smt2", std::move(Guards)},
      {"candidate_guard_smt2", expressionSMT(Region.CandidateGuard)},
      {"byte_symbols", std::move(Symbols)}};
}

const char *pathReason(const PathResult &Path) {
  switch (Path.Status) {
  case PathStatus::Completed:
    return "NORMAL-RETURN";
  case PathStatus::Incomplete:
    return toReasonCode(Path.IncompleteReason);
  case PathStatus::Error:
    return toReasonCode(Path.ErrorReason);
  case PathStatus::InternalError:
    return toReasonCode(Path.InternalReason);
  }
  return "INTERNAL-UNKNOWN-REASON";
}

} // namespace

std::string writeM5Artifacts(const std::filesystem::path &Directory,
                             const llvm::Module &Module,
                             const M5Analysis &Analysis,
                             const M5ArtifactContext &Context) {
  namespace fs = std::filesystem;
  std::error_code Error;
  fs::create_directories(Directory, Error);
  if (Error)
    return "cannot create result directory: " + Error.message();
  // Invalidating an old result precedes every other mutation in this run.
  fs::remove(Directory / "result.json", Error);
  if (Error)
    return "cannot invalidate old result.json: " + Error.message();
  for (const char *Name : {"queries", "counterexamples", "replay"}) {
    fs::create_directories(Directory / Name, Error);
    if (Error)
      return std::string("cannot create ") + Name + ": " + Error.message();
  }

  std::map<std::string, std::string> Checksums;
  auto Write = [&](const char *Name, const std::string &Body) {
    if (std::string Failure = publish(Directory / Name, Body); !Failure.empty())
      return Failure;
    Checksums.emplace(Name, hashBody(Body));
    return std::string();
  };
  if (!Analysis.Relation.SMTLib.empty()) {
    if (std::string Failure = Write("relation.smt2", Analysis.Relation.SMTLib);
        !Failure.empty())
      return Failure;
  } else {
    fs::remove(Directory / "relation.smt2", Error);
    if (Error)
      return "cannot remove old relation.smt2: " + Error.message();
  }
  if (Analysis.CounterexampleJSON) {
    if (std::string Failure =
            Write("counterexample.json", *Analysis.CounterexampleJSON);
        !Failure.empty())
      return Failure;
    if (std::string Failure =
            publish(Directory / "counterexamples" / "witness.json",
                    *Analysis.CounterexampleJSON);
        !Failure.empty())
      return Failure;
    Checksums.emplace("counterexamples/witness.json",
                      hashBody(*Analysis.CounterexampleJSON));
  } else {
    fs::remove(Directory / "counterexample.json", Error);
    if (Error)
      return "cannot remove old counterexample.json: " + Error.message();
    fs::remove(Directory / "counterexamples" / "witness.json", Error);
    if (Error)
      return "cannot remove old witness.json: " + Error.message();
  }

  const ExecutionStatistics &Stats = Analysis.Execution.Statistics;
  llvm::json::Object Metrics{
      {"states_created", static_cast<std::int64_t>(Stats.StatesCreated)},
      {"forks", static_cast<std::int64_t>(Stats.Forks)},
      {"pruned_paths", static_cast<std::int64_t>(Stats.PrunedPaths)},
      {"assumption_pruned_paths",
       static_cast<std::int64_t>(Stats.AssumptionPrunedPaths)},
      {"executed_instructions",
       static_cast<std::int64_t>(Stats.ExecutedInstructions)},
      {"solver_queries", static_cast<std::int64_t>(Stats.SolverQueries)}};
  std::string MetricsBody;
  llvm::raw_string_ostream(MetricsBody)
      << llvm::json::Value(std::move(Metrics)) << '\n';
  if (std::string Failure = Write("metrics.json", MetricsBody);
      !Failure.empty())
    return Failure;

  std::string Diagnostics = Analysis.Verdict + ": " + Analysis.Reason + "\n";
  for (const PathResult &Path : Analysis.Execution.Paths)
    if (Path.Status != PathStatus::Completed)
      Diagnostics += "state " + std::to_string(Path.StateId) + ": " +
                     pathReason(Path) + " " + Path.Detail + "\n";
  if (std::string Failure = Write("diagnostics.txt", Diagnostics);
      !Failure.empty())
    return Failure;
  auto HashExisting = [&](const fs::path &Path,
                          const std::string &Relative) -> std::string {
    std::ifstream File(Path, std::ios::binary);
    if (!File)
      return "cannot read " + Path.string();
    std::string Body((std::istreambuf_iterator<char>(File)),
                     std::istreambuf_iterator<char>());
    if (!File.good() && !File.eof())
      return "cannot read complete " + Path.string();
    Checksums.emplace(Relative, hashBody(Body));
    return {};
  };
  if (fs::exists(Directory / "preflight.json"))
    if (std::string Failure =
            HashExisting(Directory / "preflight.json", "preflight.json");
        !Failure.empty())
      return Failure;
  for (fs::directory_iterator It(Directory / "queries", Error), End;
       !Error && It != End; It.increment(Error)) {
    if (!It->is_regular_file())
      continue;
    std::string Relative = "queries/" + It->path().filename().string();
    if (std::string Failure = HashExisting(It->path(), Relative);
        !Failure.empty())
      return Failure;
  }
  if (Error)
    return "cannot inspect query artifacts: " + Error.message();

  llvm::json::Array Paths;
  llvm::json::Array Descriptors;
  llvm::json::Array Aliases;
  std::map<std::string, std::size_t> IncompletenessReasons;
  std::size_t Normal = 0, Errors = 0, Incomplete = 0, Internal = 0;
  bool HaveDescriptors = false;
  for (const PathResult &Path : Analysis.Execution.Paths) {
    switch (Path.Status) {
    case PathStatus::Completed:
      ++Normal;
      break;
    case PathStatus::Error:
      ++Errors;
      break;
    case PathStatus::Incomplete:
      ++Incomplete;
      ++IncompletenessReasons[toReasonCode(Path.IncompleteReason)];
      break;
    case PathStatus::InternalError:
      ++Internal;
      break;
    }
    llvm::json::Array Bindings;
    llvm::json::Array CallStack;
    llvm::json::Array Events;
    llvm::json::Array LoopBounds;
    for (const CallFrameInfo &Frame : Path.CallStack)
      CallStack.push_back(frameJSON(Frame));
    for (const ExecutionEvent &Event : Path.Events)
      Events.push_back(eventJSON(Event));
    for (const LoopDiagnostic &Loop : Path.LoopBounds)
      LoopBounds.push_back(loopJSON(Loop));
    for (std::size_t Ordinal = 0; Ordinal < Path.Regions.size(); ++Ordinal) {
      Bindings.push_back(regionJSON(Path.Regions[Ordinal], Ordinal));
      if (Path.Status == PathStatus::Completed && !HaveDescriptors)
        Descriptors.push_back(regionJSON(Path.Regions[Ordinal], Ordinal));
    }
    if (Path.Status == PathStatus::Completed)
      HaveDescriptors = true;
    for (std::size_t Left = 0; Left < Path.Regions.size(); ++Left) {
      const auto &Input = Path.Regions[Left];
      if (!Input.Input)
        continue;
      for (std::size_t Right = 0; Right < Path.Regions.size(); ++Right) {
        const auto &Output = Path.Regions[Right];
        if (Output.Input || Input.ObjectId != Output.ObjectId)
          continue;
        const std::uint64_t Start = std::max(Input.Offset, Output.Offset);
        const std::uint64_t End =
            std::min(Input.Offset + Input.Size, Output.Offset + Output.Size);
        if (Start < End)
          Aliases.push_back(llvm::json::Object{
              {"state_id", static_cast<std::int64_t>(Path.StateId)},
              {"input", Input.Name},
              {"output", Output.Name},
              {"object_id", static_cast<std::int64_t>(Input.ObjectId)},
              {"offset", static_cast<std::int64_t>(Start)},
              {"size", static_cast<std::int64_t>(End - Start)}});
      }
    }
    Paths.push_back(llvm::json::Object{
        {"state_id", static_cast<std::int64_t>(Path.StateId)},
        {"status", toString(Path.Status)},
        {"reason", pathReason(Path)},
        {"detail", Path.Detail},
        {"loop_policy_error", Path.LoopPolicyError},
        {"unwinding_assumption", Path.UnwindingAssumption},
        {"loop", Path.Loop ? llvm::json::Value(loopJSON(*Path.Loop))
                            : llvm::json::Value(nullptr)},
        {"loop_bounds", std::move(LoopBounds)},
        {"limit_name", Path.LimitName},
        {"limit_value", Path.LimitValue
                            ? unsignedJSON(*Path.LimitValue)
                            : llvm::json::Value(nullptr)},
        {"observed_value", Path.ObservedValue
                               ? unsignedJSON(*Path.ObservedValue)
                               : llvm::json::Value(nullptr)},
        {"source", locationJSON(Path.Location)},
        {"call_stack", std::move(CallStack)},
        {"events", std::move(Events)},
        {"call_depth_limit_hit",
         Path.IncompleteReason == IncompletenessReason::CallDepthLimit},
        {"registrations", std::move(Bindings)}});
  }
  llvm::json::Object ArtifactHashes;
  llvm::json::Object ReasonCounts;
  for (const auto &[Code, Count] : IncompletenessReasons)
    ReasonCounts[Code] = static_cast<std::int64_t>(Count);
  for (const auto &[Name, Digest] : Checksums)
    ArtifactHashes[Name] = Digest;
  llvm::json::Object Limits{
      {"timeout_ms", static_cast<std::int64_t>(Context.TimeoutMs)},
      {"max_instructions_per_state",
       static_cast<std::int64_t>(Context.Options.MaxInstructionsPerState)},
      {"max_call_depth",
       static_cast<std::int64_t>(Context.Options.MaxCallDepth)},
      {"max_states", static_cast<std::int64_t>(Context.Options.MaxStates)},
      {"default_loop_backedge_bound",
       unsignedJSON(Context.Options.DefaultLoopBound)},
      {"require_local_loop_bound", Context.Options.RequireLocalLoopBound},
      {"max_wall_time_ms",
       static_cast<std::int64_t>(Context.Options.MaxWallTimeMs)},
      {"max_solver_queries",
       Context.Options.MaxSolverQueries == UINT64_MAX
           ? llvm::json::Value("unlimited")
           : llvm::json::Value(static_cast<std::int64_t>(
                 Context.Options.MaxSolverQueries))},
      {"max_pointer_candidates",
       static_cast<std::int64_t>(Context.Options.MaxPointerCandidates)},
      {"max_object_bytes",
       static_cast<std::int64_t>(Context.Options.MaxObjectBytes)},
      {"allocation_failures", Context.Options.ModelAllocationFailure}};
  llvm::json::Object Counts{
      {"normal", static_cast<std::int64_t>(Normal)},
      {"error", static_cast<std::int64_t>(Errors)},
      {"incomplete", static_cast<std::int64_t>(Incomplete)},
      {"internal_error", static_cast<std::int64_t>(Internal)}};
  llvm::json::Object Root{
      {"schema_version", 1},
      {"format", "formal-m5-result-v1"},
      {"status", Analysis.Verdict},
      {"verdict", Analysis.Verdict},
      {"relation_completeness",
       Analysis.Relation.Complete ? "COMPLETE" : "UNDER_APPROXIMATION"},
      {"relation_file",
       Analysis.Relation.SMTLib.empty() ? "" : "relation.smt2"},
      {"counterexample_file",
       Analysis.CounterexampleJSON ? "counterexample.json" : ""},
      {"reason", Analysis.Reason},
      {"loop_limit_policy", toString(Context.Options.LoopPolicy)},
      {"unwinding_assumption", Analysis.Execution.HasUnwindingAssumption},
      {"entry", Context.Entry},
      {"run_id", Context.RunId},
      {"target_triple", Module.getTargetTriple()},
      {"data_layout", Module.getDataLayoutStr()},
      {"llvm_version", LLVM_VERSION_STRING},
      {"clang_version", Context.ClangVersion},
      {"tool_version", "formal-m5-v1"},
      {"solver_version", Context.SolverVersion},
      {"host_tool", Context.HostTool},
      {"host_tool_sha256", Context.HostToolSHA256},
      {"descriptors", std::move(Descriptors)},
      {"overlap_policy", "same-kind-overlap-rejected; exact-inout-allowed"},
      {"alias_overlaps", std::move(Aliases)},
      {"path_counts", std::move(Counts)},
      {"incompleteness_reasons", std::move(ReasonCounts)},
      {"max_call_depth_observed",
       static_cast<std::int64_t>(Stats.MaximumCallDepthObserved)},
      {"paths", std::move(Paths)},
      {"semantic_options",
       llvm::json::Object{{"byte_order", Module.getDataLayout().isLittleEndian()
                                             ? "little"
                                             : "big"},
                          {"zero_size", "UNKNOWN"},
                          {"malloc_zero_size", "fresh zero-byte object"},
                          {"allocation_failure_mode",
                           Context.Options.ModelAllocationFailure
                               ? "fork null failure"
                               : "assume success"},
                          {"solver_seed", "pinned-STP-default"}}},
      {"limits", std::move(Limits)},
      {"artifacts_sha256", std::move(ArtifactHashes)}};
  if (!Analysis.Relation.JSON.empty()) {
    auto Metadata = llvm::json::parse(Analysis.Relation.JSON);
    if (!Metadata)
      return "relation metadata is malformed";
    Root["relation_metadata"] = std::move(*Metadata);
  }
  std::string JSON;
  llvm::raw_string_ostream(JSON) << llvm::json::Value(std::move(Root)) << '\n';
  return publish(Directory / "result.json", JSON);
}

} // namespace formal
