#ifndef FORMAL_SMTLIBWRITER_H
#define FORMAL_SMTLIBWRITER_H

#include "formal/Expr.h"

#include <cstddef>
#include <string>
#include <utility>
#include <vector>

namespace formal {

struct SMTLibWriterOptions {
  std::size_t MaxExpandedNodes = 1U << 20;
  std::size_t MaxOutputBytes = 64U << 20;
};

struct SMTLibArrayRequest {
  ArrayRef Root;
  std::size_t ByteCount = 0;
};

struct SMTLibQuery {
  std::vector<ExprRef> Constraints;
  std::vector<SMTLibArrayRequest> ArrayRequests;
  ExprRef Target;
  bool AssertTarget = false;
  bool NegateTarget = false;
  bool IncludeGetModel = false;
  std::vector<std::pair<std::string, std::string>> Metadata;
};

class SMTLibWriter final {
public:
  static std::string write(const ExprRef &Expression,
                           std::string ResultName = "result",
                           bool IncludeCheckSat = true,
                           SMTLibWriterOptions Options = {});

  static std::string writeQuery(const SMTLibQuery &Query,
                                SMTLibWriterOptions Options = {});
};

} // namespace formal

#endif // FORMAL_SMTLIBWRITER_H
