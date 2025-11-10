// Bytecode manipulation utilities for Python code instrumentation
//
// This module provides tools for analyzing and modifying Python bytecode,
// including branch instructions, exception tables, and line number tables.

use ahash::{AHashMap, AHashSet};
use pyo3::prelude::*;
use pyo3::types::{PyBytes, PyDict, PyList, PyModule, PyTuple};

/// Python 3.10a7 changed branch opcodes' argument to mean instruction
/// (word) offset, rather than bytecode offset.
pub fn offset2branch(offset: i32) -> i32 {
    assert!(offset % 2 == 0, "offset must be even");
    offset / 2
}

pub fn branch2offset(arg: i32) -> i32 {
    arg * 2
}

/// Returns the number of EXTENDED_ARGs needed for an argument.
pub fn arg_ext_needed(arg: i32) -> i32 {
    // Ceiling division: -((arg >> 8).bit_length() // -8)
    let shifted = arg >> 8;
    if shifted == 0 {
        return 0;
    }
    let bit_len = 32 - shifted.leading_zeros() as i32;
    -((bit_len) / -8)
}

/// Appends a (little endian) variable length unsigned integer to 'data'
pub fn append_varint(data: &mut Vec<u8>, mut n: u32) {
    while n > 0x3F {
        data.push(0x40 | (n & 0x3F) as u8);
        n >>= 6;
    }
    data.push(n as u8);
}

/// Appends a (little endian) variable length signed integer to 'data'
pub fn append_svarint(data: &mut Vec<u8>, n: i32) {
    let unsigned = if n < 0 { ((-n) << 1) | 1 } else { n << 1 };
    append_varint(data, unsigned as u32);
}

/// Encodes a (big endian) variable length unsigned integer
pub fn write_varint_be(n: u32, mark_first: Option<u8>) -> Vec<u8> {
    let mut data = Vec::new();

    if n == 0 {
        data.push(0);
        if let Some(mark) = mark_first {
            data[0] |= mark;
        }
        return data;
    }

    let top_bit = 31 - n.leading_zeros() as i32;
    let start = top_bit - (top_bit % 6);

    let mut shift = start;
    while shift > 0 {
        data.push(0x40 | ((n >> shift) & 0x3F) as u8);
        shift -= 6;
    }
    data.push((n & 0x3F) as u8);

    if let Some(mark) = mark_first {
        data[0] |= mark;
    }

    data
}

/// Decodes a (big endian) variable length unsigned integer from an iterator
pub fn read_varint_be<I>(it: &mut I) -> Option<u32>
where
    I: Iterator<Item = u8>,
{
    let mut value = 0u32;
    loop {
        let b = it.next()?;
        if b & 0x40 != 0 {
            value |= (b & 0x3F) as u32;
            value <<= 6;
        } else {
            value |= (b & 0x3F) as u32;
            return Some(value);
        }
    }
}

/// Opcodes structure holding Python opcode constants
pub struct Opcodes {
    pub extended_arg: u8,
    pub load_const: u8,
    #[allow(dead_code)] // Reserved for potential future use
    pub load_global: u8,
    #[allow(dead_code)] // Reserved for potential future use
    pub resume: u8,
    pub push_null: u8,
    pub precall: u8,
    pub call: u8,
    pub cache: u8,
    pub pop_top: u8,
    pub jump_forward: u8,
    pub nop: u8,
    pub store_name: u8,
    pub store_global: u8,
    pub inline_cache_entries: AHashMap<u8, usize>,
    #[allow(dead_code)] // Reserved for potential future use
    pub hasjrel: AHashSet<u8>,
    #[allow(dead_code)] // Reserved for potential future use
    pub hasjabs: AHashSet<u8>,
    #[allow(dead_code)] // Reserved for potential future use
    pub opname: Vec<String>,
}

impl Opcodes {
    /// Initialize opcodes from Python's dis module
    pub fn new(py: Python) -> PyResult<Self> {
        let dis = PyModule::import(py, "dis")?;
        let opmap = dis.getattr("opmap")?;

        let extended_arg = opmap.get_item("EXTENDED_ARG")?.extract::<u8>()?;
        let load_const = opmap.get_item("LOAD_CONST")?.extract::<u8>()?;
        let load_global = opmap.get_item("LOAD_GLOBAL")?.extract::<u8>()?;
        let resume = opmap.get_item("RESUME")?.extract::<u8>()?;
        let push_null = opmap.get_item("PUSH_NULL")?.extract::<u8>()?;
        let precall = opmap.get_item("PRECALL")?.extract::<u8>()?;
        let call = opmap.get_item("CALL")?.extract::<u8>()?;
        let cache = opmap.get_item("CACHE")?.extract::<u8>()?;
        let pop_top = opmap.get_item("POP_TOP")?.extract::<u8>()?;
        let jump_forward = opmap.get_item("JUMP_FORWARD")?.extract::<u8>()?;
        let nop = opmap.get_item("NOP")?.extract::<u8>()?;
        let store_name = opmap.get_item("STORE_NAME")?.extract::<u8>()?;
        let store_global = opmap.get_item("STORE_GLOBAL")?.extract::<u8>()?;

        // Convert Python dict to Rust HashMap
        let inline_cache_entries_py = dis.getattr("_inline_cache_entries")?;
        let mut inline_cache_entries = AHashMap::new();
        let cache_dict: &Bound<PyDict> = inline_cache_entries_py.cast()?;
        for item in cache_dict.items() {
            let key: u8 = item.get_item(0)?.extract()?;
            let value: usize = item.get_item(1)?.extract()?;
            inline_cache_entries.insert(key, value);
        }

        // Get hasjrel and hasjabs as sets
        let hasjrel_list = dis.getattr("hasjrel")?.extract::<Vec<u8>>()?;
        let hasjabs_list = dis.getattr("hasjabs")?.extract::<Vec<u8>>()?;
        let hasjrel: AHashSet<u8> = hasjrel_list.into_iter().collect();
        let hasjabs: AHashSet<u8> = hasjabs_list.into_iter().collect();

        // Convert Python list to Rust Vec
        let opname_py = dis.getattr("opname")?;
        let opname: Vec<String> = opname_py.extract()?;

        Ok(Opcodes {
            extended_arg,
            load_const,
            load_global,
            resume,
            push_null,
            precall,
            call,
            cache,
            pop_top,
            jump_forward,
            nop,
            store_name,
            store_global,
            inline_cache_entries,
            hasjrel,
            hasjabs,
            opname,
        })
    }

    /// Get inline cache entry count for an opcode
    pub fn get_inline_cache_entries(&self, opcode: u8) -> usize {
        *self.inline_cache_entries.get(&opcode).unwrap_or(&0)
    }

    /// Check if opcode is EXTENDED_ARG
    pub fn is_extended_arg(&self, opcode: u8) -> bool {
        opcode == self.extended_arg
    }
}

/// Emits an opcode and its (variable length) argument
pub fn opcode_arg(opcodes: &Opcodes, opcode: u8, arg: i32, min_ext: i32) -> Vec<u8> {
    let mut bytecode = Vec::new();
    let ext = std::cmp::max(arg_ext_needed(arg), min_ext);
    assert!(ext <= 3, "Too many EXTENDED_ARG needed");

    for i in 0..ext {
        bytecode.push(opcodes.extended_arg);
        bytecode.push(((arg >> ((ext - i) * 8)) & 0xFF) as u8);
    }
    bytecode.push(opcode);
    bytecode.push((arg & 0xFF) as u8);

    // Add cache entries
    let cache_count = opcodes.get_inline_cache_entries(opcode);
    for _ in 0..cache_count {
        bytecode.push(opcodes.cache);
        bytecode.push(0);
    }

    bytecode
}

/// Represents an unpacked opcode with its offset, length, opcode, and argument
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnpackedOp {
    pub offset: usize,
    pub length: usize,
    pub opcode: u8,
    pub arg: i32,
}

/// Unpacks opcodes and their arguments from bytecode
pub fn unpack_opargs(code: &[u8], opcodes: &Opcodes) -> Vec<UnpackedOp> {
    let mut result = Vec::new();
    let mut ext_arg = 0i32;
    let mut next_off = 0usize;
    let mut off = 0usize;

    while off < code.len() {
        let op = code[off];
        if opcodes.is_extended_arg(op) {
            ext_arg = (ext_arg | code[off + 1] as i32) << 8;
        } else {
            let arg = ext_arg | code[off + 1] as i32;
            let mut end_off = off + 2;

            // Skip CACHE opcodes
            while end_off < code.len() && code[end_off] == opcodes.cache {
                end_off += 2;
            }

            result.push(UnpackedOp {
                offset: next_off,
                length: end_off - next_off,
                opcode: op,
                arg,
            });

            ext_arg = 0;
            next_off = end_off;
        }
        off += 2;
    }

    result
}

/// Calculates the maximum stack size for code to execute.
/// Assumes linear execution (i.e., not things like a loop pushing to the stack).
pub fn calc_max_stack(py: Python, code: &[u8], opcodes: &Opcodes) -> PyResult<i32> {
    let dis = PyModule::import(py, "dis")?;
    let stack_effect_fn = dis.getattr("stack_effect")?;
    let have_argument = dis.getattr("HAVE_ARGUMENT")?.extract::<u8>()?;

    let mut max_stack = 0i32;
    let mut stack = 0i32;

    for op_data in unpack_opargs(code, opcodes) {
        let arg_value = if op_data.opcode >= have_argument {
            Some(op_data.arg)
        } else {
            None
        };

        let effect = if let Some(arg) = arg_value {
            stack_effect_fn.call1((op_data.opcode, arg))?
        } else {
            stack_effect_fn.call1((op_data.opcode,))?
        };

        stack += effect.extract::<i32>()?;
        max_stack = max_stack.max(stack);
    }

    Ok(max_stack)
}

/// Describes a branch instruction
#[pyclass]
#[derive(Clone, PartialEq, Eq)]
pub struct Branch {
    #[pyo3(get, set)]
    pub offset: i32,
    #[pyo3(get, set)]
    pub length: i32,
    #[pyo3(get, set)]
    pub opcode: u8,
    #[pyo3(get, set)]
    pub is_relative: bool,
    #[pyo3(get, set)]
    pub is_backward: bool,
    #[pyo3(get, set)]
    pub target: i32,
}

#[pymethods]
impl Branch {
    #[new]
    fn new(py: Python, offset: i32, length: i32, opcode: u8, arg: i32) -> PyResult<Self> {
        let dis = PyModule::import(py, "dis")?;
        let hasjrel = dis.getattr("hasjrel")?.extract::<Vec<u8>>()?;
        let opname = dis.getattr("opname")?.extract::<Py<PyList>>()?;

        let is_relative = hasjrel.contains(&opcode);
        let opname_str = opname
            .bind(py)
            .get_item(opcode as usize)?
            .extract::<String>()?;
        let is_backward = opname_str.contains("JUMP_BACKWARD");

        let target = if !is_relative {
            branch2offset(arg)
        } else {
            offset + length + branch2offset(if is_backward { -arg } else { arg })
        };

        Ok(Branch {
            offset,
            length,
            opcode,
            is_relative,
            is_backward,
            target,
        })
    }

    /// Returns this branch's opcode argument
    fn arg(&self) -> i32 {
        if self.is_relative {
            offset2branch((self.target - (self.offset + self.length)).abs())
        } else {
            offset2branch(self.target)
        }
    }

    /// Adjusts this branch after a code insertion
    fn adjust(&mut self, insert_offset: i32, insert_length: i32) {
        if self.offset >= insert_offset {
            self.offset += insert_length;
        }
        if self.target > insert_offset {
            self.target += insert_length;
        }
    }

    /// Adjusts this branch's opcode length, if needed.
    /// Returns the number of bytes by which the length increased.
    fn adjust_length(&mut self) -> i32 {
        let length_needed = 2 + 2 * arg_ext_needed(self.arg());
        let change = std::cmp::max(0, length_needed - self.length);
        if change > 0 {
            if self.target > self.offset {
                self.target += change;
            }
            self.length = length_needed;
        }
        change
    }

    /// Emits this branch's code
    fn code(&self, py: Python) -> PyResult<Py<PyBytes>> {
        let opcodes = Opcodes::new(py)?;
        assert!(
            self.length >= 2 + 2 * arg_ext_needed(self.arg()),
            "Branch length too small"
        );
        let bytecode = opcode_arg(&opcodes, self.opcode, self.arg(), (self.length - 2) / 2);
        Ok(PyBytes::new(py, &bytecode).into())
    }
}

// Non-Python methods for internal Rust use
impl Branch {
    /// Internal method to create a Branch without calling Python
    fn new_internal(
        offset: i32,
        length: i32,
        opcode: u8,
        arg: i32,
        hasjrel: &AHashSet<u8>,
        opname: &[String],
    ) -> Self {
        let is_relative = hasjrel.contains(&opcode);
        let opname_str = opname
            .get(opcode as usize)
            .map(|s| s.as_str())
            .unwrap_or("");
        let is_backward = opname_str.contains("JUMP_BACKWARD");

        let target = if !is_relative {
            branch2offset(arg)
        } else {
            offset + length + branch2offset(if is_backward { -arg } else { arg })
        };

        Branch {
            offset,
            length,
            opcode,
            is_relative,
            is_backward,
            target,
        }
    }

    /// Finds all Branches in bytecode
    pub fn from_code_impl(co_code: &[u8], opcodes: &Opcodes) -> Vec<Branch> {
        let mut branch_opcodes = AHashSet::new();
        for op in &opcodes.hasjrel {
            branch_opcodes.insert(*op);
        }
        for op in &opcodes.hasjabs {
            branch_opcodes.insert(*op);
        }

        let unpacked = unpack_opargs(co_code, opcodes);

        let mut branches = Vec::new();
        for op_data in unpacked {
            if branch_opcodes.contains(&op_data.opcode) {
                branches.push(Branch::new_internal(
                    op_data.offset as i32,
                    op_data.length as i32,
                    op_data.opcode,
                    op_data.arg,
                    &opcodes.hasjrel,
                    &opcodes.opname,
                ));
            }
        }

        branches
    }
}

/// Represents an entry from Python 3.11+'s exception table
#[pyclass]
pub struct ExceptionTableEntry {
    #[pyo3(get, set)]
    pub start: i32,
    #[pyo3(get, set)]
    pub end: i32,
    #[pyo3(get, set)]
    pub target: i32,
    #[pyo3(get, set)]
    pub other: u32,
}

#[pymethods]
impl ExceptionTableEntry {
    #[new]
    fn new(start: i32, end: i32, target: i32, other: u32) -> Self {
        ExceptionTableEntry {
            start,
            end,
            target,
            other,
        }
    }

    /// Adjusts this exception table entry, handling a code insertion
    fn adjust(&mut self, insert_offset: i32, insert_length: i32) {
        if insert_offset <= self.start {
            self.start += insert_length;
        }
        if insert_offset < self.end {
            self.end += insert_length;
        }
        if insert_offset < self.target {
            self.target += insert_length;
        }
    }

    /// Generates an exception table from a list of entries
    #[staticmethod]
    fn make_exceptiontable(
        py: Python,
        entries: Vec<Py<ExceptionTableEntry>>,
    ) -> PyResult<Py<PyBytes>> {
        let mut table = Vec::new();

        for entry_py in entries {
            let entry = entry_py.borrow(py);
            table.extend(write_varint_be(
                offset2branch(entry.start) as u32,
                Some(0x80),
            ));
            table.extend(write_varint_be(
                offset2branch(entry.end - entry.start) as u32,
                None,
            ));
            table.extend(write_varint_be(offset2branch(entry.target) as u32, None));
            table.extend(write_varint_be(entry.other, None));
        }

        Ok(PyBytes::new(py, &table).into())
    }
}

// Non-Python methods for internal Rust use
impl ExceptionTableEntry {
    /// Returns a list of exception table entries from exception table bytes
    pub fn from_code_impl(exceptiontable: &[u8]) -> Result<Vec<ExceptionTableEntry>, String> {
        let mut entries = Vec::new();
        let mut it = exceptiontable.iter().copied();

        while let Some(start_val) = read_varint_be(&mut it) {
            let length = read_varint_be(&mut it)
                .ok_or_else(|| "Invalid exception table: missing length".to_string())?;
            let start = branch2offset(start_val as i32);
            let end = start + branch2offset(length as i32);
            let target = branch2offset(
                read_varint_be(&mut it)
                    .ok_or_else(|| "Invalid exception table: missing target".to_string())?
                    as i32,
            );
            let other = read_varint_be(&mut it)
                .ok_or_else(|| "Invalid exception table: missing other".to_string())?;

            entries.push(ExceptionTableEntry {
                start,
                end,
                target,
                other,
            });
        }

        Ok(entries)
    }
}

/// Describes a range of bytecode offsets belonging to a line of Python source code
#[pyclass]
pub struct LineEntry {
    #[pyo3(get, set)]
    pub start: i32,
    #[pyo3(get, set)]
    pub end: i32,
    #[pyo3(get, set)]
    pub number: Option<i32>,
}

#[pymethods]
impl LineEntry {
    #[new]
    fn new(start: i32, end: i32, number: Option<i32>) -> Self {
        LineEntry { start, end, number }
    }

    fn __repr__(&self) -> String {
        if let Some(num) = self.number {
            format!(
                "LineEntry(start={},end={},number={})",
                self.start, self.end, num
            )
        } else {
            format!(
                "LineEntry(start={},end={},number=None)",
                self.start, self.end
            )
        }
    }

    /// Adjusts this line after a code insertion
    fn adjust(&mut self, insert_offset: i32, insert_length: i32) {
        if self.start > insert_offset {
            self.start += insert_length;
        }
        if self.end > insert_offset {
            self.end += insert_length;
        }
    }

    /// Generates the positions table used by Python 3.11+ to map offsets to line numbers
    #[staticmethod]
    fn make_linetable(
        py: Python,
        firstlineno: i32,
        lines: Vec<Py<LineEntry>>,
    ) -> PyResult<Py<PyBytes>> {
        let mut linetable = Vec::new();
        let mut prev_end = 0i32;
        let mut prev_number = firstlineno;

        for line_py in lines {
            let line = line_py.borrow(py);

            if line.number.is_none() {
                let mut bytecodes = (line.end - prev_end) / 2;
                while bytecodes > 0 {
                    linetable.push(0x80 | (15 << 3) | (std::cmp::min(bytecodes, 8) - 1) as u8);
                    bytecodes -= 8;
                }
            } else {
                if prev_end < line.start {
                    let mut bytecodes = (line.start - prev_end) / 2;
                    while bytecodes > 0 {
                        linetable.push(0x80 | (15 << 3) | (std::cmp::min(bytecodes, 8) - 1) as u8);
                        bytecodes -= 8;
                    }
                }

                let mut line_delta = line.number.unwrap() - prev_number;
                let mut bytecodes = (line.end - line.start) / 2;
                while bytecodes > 0 {
                    linetable.push(0x80 | (13 << 3) | (std::cmp::min(bytecodes, 8) - 1) as u8);
                    append_svarint(&mut linetable, line_delta);
                    line_delta = 0;
                    bytecodes -= 8;
                }

                prev_number = line.number.unwrap();
            }

            prev_end = line.end;
        }

        Ok(PyBytes::new(py, &linetable).into())
    }
}

// Non-Python methods for internal Rust use
impl LineEntry {
    /// Extracts a list of line entries from line starts
    pub fn from_code_impl(line_starts: Vec<(i32, i32)>, code_len: i32) -> Vec<LineEntry> {
        let mut lines = Vec::new();
        let mut last: Option<(i32, i32)> = None;

        for tuple in line_starts {
            if let Some((last_off, last_line)) = last {
                lines.push(LineEntry {
                    start: last_off,
                    end: tuple.0,
                    number: Some(last_line),
                });
            }
            last = Some(tuple);
        }

        if let Some((last_off, last_line)) = last {
            lines.push(LineEntry {
                start: last_off,
                end: code_len,
                number: Some(last_line),
            });
        }

        lines
    }
}

/// Implements a bytecode editor
#[pyclass]
pub struct Editor {
    orig_code: Py<PyAny>,
    consts: Option<Vec<Py<PyAny>>>,
    patch: Option<Vec<u8>>,
    branches: Option<Vec<Branch>>,
    ex_table: Vec<ExceptionTableEntry>,
    lines: Vec<LineEntry>,
    inserts: Vec<i32>,
    max_addtl_stack: i32,
    finished: bool,
}

#[pymethods]
impl Editor {
    #[new]
    fn new(code: Py<PyAny>) -> Self {
        Editor {
            orig_code: code,
            consts: None,
            patch: None,
            branches: None,
            ex_table: Vec::new(),
            lines: Vec::new(),
            inserts: Vec::new(),
            max_addtl_stack: 0,
            finished: false,
        }
    }

    /// Sets a constant
    fn set_const(&mut self, py: Python, index: usize, value: Py<PyAny>) -> PyResult<()> {
        if self.consts.is_none() {
            let co_consts = self.orig_code.bind(py).getattr("co_consts")?;
            let co_consts_tuple: &pyo3::Bound<PyTuple> = co_consts.cast()?;
            self.consts = Some(co_consts_tuple.iter().map(|x| x.unbind()).collect());
        }

        if let Some(ref mut consts) = self.consts
            && index < consts.len()
        {
            consts[index] = value;
        }

        Ok(())
    }

    /// Adds a constant
    fn add_const(&mut self, py: Python, value: Py<PyAny>) -> PyResult<usize> {
        if self.consts.is_none() {
            let co_consts = self.orig_code.bind(py).getattr("co_consts")?;
            let co_consts_tuple: &pyo3::Bound<PyTuple> = co_consts.cast()?;
            self.consts = Some(co_consts_tuple.iter().map(|x| x.unbind()).collect());
        }

        if let Some(ref mut consts) = self.consts {
            consts.push(value);
            Ok(consts.len() - 1)
        } else {
            Err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(
                "Failed to initialize consts",
            ))
        }
    }

    /// Inserts a function call.
    /// repl_length, if passed, indicates the number of bytes to replace at that offset.
    fn insert_function_call(
        &mut self,
        py: Python,
        offset: i32,
        function: i32,
        args: Vec<i32>,
        repl_length: Option<i32>,
    ) -> PyResult<i32> {
        assert!(!self.finished, "Editor already finished");

        let repl_length = repl_length.unwrap_or(0);

        // Create Opcodes once
        let opcodes = Opcodes::new(py)?;

        if self.patch.is_none() {
            let co_code_attr = self.orig_code.bind(py).getattr("co_code")?;
            let co_code = co_code_attr.extract::<Vec<u8>>()?;
            self.patch = Some(co_code);
        }

        if self.branches.is_none() {
            let code_bound = self.orig_code.bind(py);

            // Extract bytecode
            let co_code_attr = code_bound.getattr("co_code")?;
            let co_code = co_code_attr.extract::<&[u8]>()?;
            let code_len = co_code.len() as i32;

            // Extract exception table
            let exceptiontable_attr = code_bound.getattr("co_exceptiontable")?;
            let exceptiontable = exceptiontable_attr.extract::<&[u8]>()?;

            // Extract line starts using dis.findlinestarts
            let dis = PyModule::import(py, "dis")?;
            let findlinestarts_fn = dis.getattr("findlinestarts")?;
            let line_iter = findlinestarts_fn.call1((code_bound,))?;
            let mut line_starts = Vec::new();
            for item in line_iter.try_iter()? {
                let tuple = item?.extract::<(i32, i32)>()?;
                line_starts.push(tuple);
            }

            // Call refactored from_code_impl functions with native Rust data
            self.branches = Some(Branch::from_code_impl(co_code, &opcodes));
            self.ex_table = ExceptionTableEntry::from_code_impl(exceptiontable)
                .map_err(PyErr::new::<pyo3::exceptions::PyValueError, _>)?;
            self.lines = LineEntry::from_code_impl(line_starts, code_len);
        }
        let mut insert = vec![
            // NOP for disabling
            opcodes.nop,
            0,
            // PUSH_NULL
            opcodes.push_null,
            0,
        ];

        // LOAD_CONST for function
        insert.extend(opcode_arg(&opcodes, opcodes.load_const, function, 0));

        // LOAD_CONST for each argument
        for a in &args {
            insert.extend(opcode_arg(&opcodes, opcodes.load_const, *a, 0));
        }

        // PRECALL
        insert.extend(opcode_arg(&opcodes, opcodes.precall, args.len() as i32, 0));

        // CALL
        insert.extend(opcode_arg(&opcodes, opcodes.call, args.len() as i32, 0));

        // POP_TOP (ignore return)
        insert.extend([opcodes.pop_top, 0]);

        let len_insert = insert.len() as i32;
        if len_insert < repl_length {
            return Err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(
                "shrinking insertions not (yet?) supported.",
            ));
        }

        let jump_arg = offset2branch(len_insert - 2);
        if jump_arg > 255 {
            return Err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(
                "Jump argument too large for single byte",
            ));
        }
        insert[1] = jump_arg as u8;

        self.max_addtl_stack =
            std::cmp::max(self.max_addtl_stack, calc_max_stack(py, &insert, &opcodes)?);

        // Replace in patch
        if let Some(ref mut patch) = self.patch {
            let offset_usize = offset as usize;
            let repl_len_usize = repl_length as usize;
            patch.splice(offset_usize..offset_usize + repl_len_usize, insert);
        }

        let bytes_added = len_insert - repl_length;

        // Adjust lines
        for line in &mut self.lines {
            line.adjust(offset, bytes_added);
        }

        // Adjust branches
        if let Some(ref mut branches) = self.branches {
            for b in branches {
                b.adjust(offset, bytes_added);
            }
        }

        // Adjust exception table
        for e in &mut self.ex_table {
            e.adjust(offset, bytes_added);
        }

        self.inserts.push(offset);

        Ok(bytes_added)
    }

    /// Finds STORE_NAME assignments to the given variable,
    /// coming from an immediately preceding LOAD_CONST.
    fn find_const_assignments(
        &self,
        py: Python,
        var_name: String,
        start: Option<i32>,
        end: Option<i32>,
    ) -> PyResult<Vec<(i32, i32, i32)>> {
        let co_code_attr = self.orig_code.bind(py).getattr("co_code")?;
        let co_code = co_code_attr.extract::<&[u8]>()?;
        let co_names_bound = self.orig_code.bind(py).getattr("co_names")?;
        let co_names: &pyo3::Bound<PyTuple> = co_names_bound.cast()?;

        let start = start.unwrap_or(0) as usize;
        let end = end.unwrap_or(co_code.len() as i32) as usize;
        let slice = &co_code[start..end];

        let opcodes = Opcodes::new(py)?;
        let unpacked = unpack_opargs(slice, &opcodes);

        let mut results = Vec::new();
        let mut load_off: Option<(usize, i32)> = None;

        for op_data in unpacked {
            if op_data.opcode == opcodes.load_const {
                load_off = Some((op_data.offset, op_data.arg));
            } else if let Some((lo, const_arg)) = load_off {
                if (op_data.opcode == opcodes.store_name || op_data.opcode == opcodes.store_global)
                    && co_names
                        .get_item(op_data.arg as usize)?
                        .extract::<String>()?
                        == var_name
                {
                    results.push((
                        (lo + start) as i32,
                        (op_data.offset + op_data.length + start) as i32,
                        const_arg,
                    ));
                }
                load_off = None;
            }
        }

        Ok(results)
    }

    /// Returns const indices for an inserted function and for its arguments,
    /// or None if an inserted function isn't recognized.
    fn get_inserted_function(&self, py: Python, offset: i32) -> PyResult<Option<Vec<i32>>> {
        let code_vec;
        let code = if let Some(ref patch) = self.patch {
            patch.as_slice()
        } else {
            let co_code_attr = self.orig_code.bind(py).getattr("co_code")?;
            code_vec = co_code_attr.extract::<Vec<u8>>()?;
            code_vec.as_slice()
        };

        let offset_usize = offset as usize;
        if offset_usize >= code.len() {
            return Ok(None);
        }

        let opcodes = Opcodes::new(py)?;
        if code[offset_usize] != opcodes.nop {
            return Ok(None);
        }

        let unpacked = unpack_opargs(&code[offset_usize..], &opcodes);
        let mut it = unpacked.into_iter();

        // Skip NOP
        it.next();

        // Check for PUSH_NULL
        if let Some(op) = it.next() {
            if op.opcode != opcodes.push_null {
                return Ok(None);
            }
        } else {
            return Ok(None);
        }

        // Check for LOAD_CONST (function)
        let mut f_args = Vec::new();
        if let Some(op) = it.next() {
            if op.opcode == opcodes.load_const {
                f_args.push(op.arg);
            } else {
                return Ok(None);
            }
        } else {
            return Ok(None);
        }

        // Collect all LOAD_CONST args
        for op in it {
            if op.opcode == opcodes.load_const {
                f_args.push(op.arg);
            } else {
                break;
            }
        }

        Ok(Some(f_args))
    }

    /// Disables an inserted function at a given offset
    fn disable_inserted_function(&mut self, py: Python, offset: i32) -> PyResult<()> {
        assert!(!self.finished, "Editor already finished");

        if self.patch.is_none() {
            let co_code_attr = self.orig_code.bind(py).getattr("co_code")?;
            let co_code = co_code_attr.extract::<Vec<u8>>()?;
            self.patch = Some(co_code);
        }

        let opcodes = Opcodes::new(py)?;
        if let Some(ref mut patch) = self.patch {
            let offset_usize = offset as usize;
            assert!(patch[offset_usize] == opcodes.nop, "Expected NOP at offset");
            patch[offset_usize] = opcodes.jump_forward;
        }

        Ok(())
    }

    fn _finish(&mut self, py: Python) -> PyResult<()> {
        if self.finished {
            return Ok(());
        }

        self.finished = true;

        if let Some(ref mut branches) = self.branches {
            // A branch's new target may now require more EXTENDED_ARG opcodes to be expressed.
            // Inserting space for those may in turn trigger needing more space for others...
            let mut any_adjusted = true;
            while any_adjusted {
                any_adjusted = false;

                let mut adjustments = Vec::new();
                for (idx, b) in branches.iter_mut().enumerate() {
                    let change = b.adjust_length();
                    if change > 0 {
                        adjustments.push((idx, b.offset, change));
                    }
                }

                for (branch_idx, offset, change) in adjustments {
                    if let Some(ref mut patch) = self.patch {
                        // Insert zeros at the branch offset
                        let offset_usize = offset as usize;
                        patch.splice(offset_usize..offset_usize, vec![0; change as usize]);
                    }

                    // Adjust all other branches
                    for (idx, c) in branches.iter_mut().enumerate() {
                        if idx != branch_idx {
                            c.adjust(offset, change);
                        }
                    }

                    // Adjust lines
                    for line in &mut self.lines {
                        line.adjust(offset, change);
                    }

                    // Adjust exception table
                    for e in &mut self.ex_table {
                        e.adjust(offset, change);
                    }

                    // Adjust inserts
                    for insert in &mut self.inserts {
                        if offset <= *insert {
                            *insert += change;
                        }
                    }

                    any_adjusted = true;
                }
            }

            // Write branch code
            if let Some(ref mut patch) = self.patch {
                for b in branches.iter() {
                    let code_py = b.code(py)?;
                    let code_bytes_bound = code_py.bind(py);
                    let code_bytes = code_bytes_bound.extract::<&[u8]>()?;
                    let offset_usize = b.offset as usize;
                    let length_usize = b.length as usize;

                    assert!(
                        patch[offset_usize + length_usize - 2] == b.opcode,
                        "Branch opcode mismatch"
                    );

                    patch[offset_usize..offset_usize + length_usize]
                        .copy_from_slice(&code_bytes[..length_usize]);
                }
            }
        }

        Ok(())
    }

    /// Gets the list of insert offsets
    fn get_inserts(&mut self, py: Python) -> PyResult<Vec<i32>> {
        self._finish(py)?;
        Ok(self.inserts.clone())
    }

    /// Finishes editing bytecode, returning a new code object
    fn finish(&mut self, py: Python) -> PyResult<Py<PyAny>> {
        self._finish(py)?;

        if self.patch.is_none() && self.consts.is_none() {
            return Ok(self.orig_code.clone_ref(py));
        }

        let replace_dict = PyDict::new(py);

        if let Some(ref consts) = self.consts {
            let consts_refs: Vec<_> = consts.iter().map(|c| c.bind(py)).collect();
            let consts_tuple = PyTuple::new(py, &consts_refs)?;
            replace_dict.set_item("co_consts", consts_tuple)?;
        }

        if self.max_addtl_stack > 0 {
            let co_stacksize = self
                .orig_code
                .bind(py)
                .getattr("co_stacksize")?
                .extract::<i32>()?;
            replace_dict.set_item("co_stacksize", co_stacksize + self.max_addtl_stack)?;
        }

        if let Some(ref patch) = self.patch {
            let code_bytes = PyBytes::new(py, patch);
            replace_dict.set_item("co_code", code_bytes)?;
        }

        if self.branches.is_some() {
            let firstlineno = self
                .orig_code
                .bind(py)
                .getattr("co_firstlineno")?
                .extract::<i32>()?;
            let lines_py: Vec<Py<LineEntry>> = self
                .lines
                .iter()
                .map(|l| {
                    Py::new(
                        py,
                        LineEntry {
                            start: l.start,
                            end: l.end,
                            number: l.number,
                        },
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            let linetable = LineEntry::make_linetable(py, firstlineno, lines_py)?;
            replace_dict.set_item("co_linetable", linetable)?;

            let ex_entries_py: Vec<Py<ExceptionTableEntry>> = self
                .ex_table
                .iter()
                .map(|e| {
                    Py::new(
                        py,
                        ExceptionTableEntry {
                            start: e.start,
                            end: e.end,
                            target: e.target,
                            other: e.other,
                        },
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            let exceptiontable = ExceptionTableEntry::make_exceptiontable(py, ex_entries_py)?;
            replace_dict.set_item("co_exceptiontable", exceptiontable)?;
        }

        let replace_fn = self.orig_code.bind(py).getattr("replace")?;
        let new_code = replace_fn.call((), Some(&replace_dict))?;

        Ok(new_code.into())
    }
}
