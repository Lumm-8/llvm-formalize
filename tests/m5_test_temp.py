"""Keep M5 test queries and artifacts when an assertion fails."""

import shutil
import sys
import tempfile


class RetainedTemporaryDirectory:
    def __init__(self, prefix):
        self.path = tempfile.mkdtemp(prefix=prefix)

    def __enter__(self):
        return self.path

    def __exit__(self, exc_type, exc_value, traceback):
        if exc_type is None:
            shutil.rmtree(self.path)
        else:
            print(f"retained failing M5 queries and artifacts: {self.path}",
                  file=sys.stderr)
        return False
