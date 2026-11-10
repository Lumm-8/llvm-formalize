#include "formal/Relation.h"

#include "gtest/gtest.h"

using namespace formal;

TEST(FormalRelation, RejectsUndeclaredInternalSymbol) {
  ExprContext Expressions;
  ExecutionResult Result;
  Result.Status = ExecutionStatus::Complete;
  PathResult Path;
  Path.Status = PathStatus::Completed;
  Path.Constraints.push_back(Expressions.booleanSymbol("hidden"));
  Result.Paths.push_back(std::move(Path));

  RelationArtifact Artifact = exportRelation(Result, Expressions);
  EXPECT_FALSE(Artifact.Complete);
  EXPECT_TRUE(Artifact.SMTLib.empty());
  EXPECT_NE(Artifact.Reason.find("undeclared internal symbol"),
            std::string::npos);
}

TEST(FormalRelation, DeclaresRegisteredInputEvenWhenUnused) {
  ExprContext Expressions;
  ExecutionResult Result;
  Result.Status = ExecutionStatus::Complete;
  PathResult Path;
  Path.Status = PathStatus::Completed;
  PathResult::RegisteredRegion Input;
  Input.Input = true;
  Input.Name = "x";
  Input.Size = 1;
  Input.Bytes.push_back(Expressions.symbol(8, "input.x.0"));
  Path.Regions.push_back(std::move(Input));
  PathResult::RegisteredRegion Output;
  Output.Name = "y";
  Output.Size = 1;
  Output.Bytes.push_back(Expressions.constant(8, 7));
  Path.Regions.push_back(std::move(Output));
  Result.Paths.push_back(std::move(Path));

  RelationArtifact Artifact = exportRelation(Result, Expressions);
  ASSERT_TRUE(Artifact.Complete) << Artifact.Reason;
  EXPECT_NE(Artifact.SMTLib.find("(declare-fun |input.x.0|"),
            std::string::npos);
  EXPECT_NE(Artifact.SMTLib.find("(declare-fun |output.y.0|"),
            std::string::npos);
}

TEST(FormalRelation, ErrorWitnessMustBeClosedOverRegisteredInputs) {
  ExprContext Expressions;
  PathResult Path;
  Path.Status = PathStatus::Error;
  Path.ErrorModel = Model{};
  Path.Constraints.push_back(Expressions.booleanSymbol("hidden"));
  EXPECT_FALSE(hasClosedRegisteredErrorPath(Path));
  Path.Constraints.clear();
  Path.Constraints.push_back(Expressions.eq(Expressions.symbol(8, "input.x.0"),
                                            Expressions.constant(8, 1)));
  PathResult::RegisteredRegion Input;
  Input.Input = true;
  Input.Name = "x";
  Input.Size = 1;
  Path.Regions.push_back(std::move(Input));
  EXPECT_TRUE(hasClosedRegisteredErrorPath(Path));
}

TEST(FormalRelation, CompletedPathsRemainAnExplicitUnderApproximation) {
  ExprContext Expressions;
  ExecutionResult Result;
  Result.Status = ExecutionStatus::Unknown;
  PathResult Completed;
  Completed.Status = PathStatus::Completed;
  PathResult::RegisteredRegion Input;
  Input.Input = true;
  Input.Name = "x";
  Input.Size = 1;
  Input.Bytes.push_back(Expressions.symbol(8, "input.x.0"));
  Completed.Regions.push_back(std::move(Input));
  PathResult::RegisteredRegion Output;
  Output.Name = "y";
  Output.Size = 1;
  Output.Bytes.push_back(Expressions.constant(8, 7));
  Completed.Regions.push_back(std::move(Output));
  Result.Paths.push_back(std::move(Completed));
  PathResult Incomplete;
  Incomplete.Status = PathStatus::Incomplete;
  Incomplete.IncompleteReason = IncompletenessReason::UnsupportedCall;
  Result.Paths.push_back(std::move(Incomplete));

  RelationArtifact Artifact = exportRelation(Result, Expressions);
  EXPECT_FALSE(Artifact.Complete);
  EXPECT_TRUE(Artifact.UnderApproximation) << Artifact.Reason;
  EXPECT_NE(Artifact.SMTLib.find("|formal_relation|"), std::string::npos);
  EXPECT_NE(Artifact.JSON.find("UNDER_APPROXIMATION"), std::string::npos);
}

TEST(FormalRelation, NoNormalReturnHasFalseRelation) {
  ExprContext Expressions;
  ExecutionResult Result;
  Result.Status = ExecutionStatus::Unknown;
  PathResult Incomplete;
  Incomplete.Status = PathStatus::Incomplete;
  Result.Paths.push_back(std::move(Incomplete));
  RelationArtifact Artifact = exportRelation(Result, Expressions);
  EXPECT_FALSE(Artifact.Complete);
  EXPECT_TRUE(Artifact.UnderApproximation);
  EXPECT_NE(Artifact.SMTLib.find("false"), std::string::npos);
}
