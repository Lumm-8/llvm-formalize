import os
import lit.formats

config.name = "FormalPass-M0-M1"
config.test_format = lit.formats.ShTest(True)
config.suffixes = [".ll"]
config.test_source_root = os.path.join(config.formal_source_root, "tests", "ir")
config.test_exec_root = os.path.join(config.formal_binary_root, "tests", "lit")

config.substitutions.append(("%opt", config.formal_opt))
config.substitutions.append(("%FileCheck", config.formal_filecheck))
config.substitutions.append(("%not", config.formal_not))
config.substitutions.append(
    ("%plugin", config.formal_plugin + " -formal-allow-unversioned-ir"))
config.substitutions.append(("%bare-plugin", config.formal_plugin))
