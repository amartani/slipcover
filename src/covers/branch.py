# Import from Rust implementation
# The preinstrument function is now implemented in Rust using ruff's parser
from .covers_core import preinstrument

# Keep the constant for backwards compatibility
EXIT = 0

# Note: The preinstrument function is now implemented in Rust (src_rust/lib.rs)
# It uses ruff's parser instead of Python's ast module for better performance
# and to avoid importing the ast module from Rust code.
