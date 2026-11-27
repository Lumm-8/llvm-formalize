import os
import lit.formats

config.name = "FormalPass-M0-M1-M3"
config.test_format = lit.formats.ShTest(True)
config.suffixes = [".ll"]
config.excludes = ["differential.ll"]
config.test_source_root = os.path.join(config.formal_source_root, "tests", "ir")
config.test_exec_root = lit_config.params.get(
    "formal_exec_root", os.path.join(config.formal_binary_root, "tests", "lit"))
for option in ("FORMAL_QUERY_CACHE", "FORMAL_EXPR_REWRITE", "FORMAL_STATE_MERGE",
               "FORMAL_TEST_ARTIFACT_ROOT", "FORMAL_TEST_CASE"):
    if option in os.environ:
        config.environment[option] = os.environ[option]

config.substitutions.append(("%opt", config.formal_opt))
config.substitutions.append(("%FileCheck", config.formal_filecheck))
config.substitutions.append(("%not", config.formal_not))
config.substitutions.append(("%scalar-executor", config.formal_scalar_executor))
config.substitutions.append(
    ("%plugin", config.formal_plugin + " -formal-allow-unversioned-ir"))
config.substitutions.append(("%bare-plugin", config.formal_plugin))
config.substitutions.append(
    ("%merge-mode", "MERGED" if os.environ.get("FORMAL_STATE_MERGE") == "1"
     else "BASELINE"))
