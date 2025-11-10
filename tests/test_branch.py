import pytest
import ast
import covers.branch as br


def ast_parse(s):
    import inspect

    return ast.parse(inspect.cleandoc(s))


def get_branches(code):
    from covers.covers import branches_from_code

    return sorted(branches_from_code(code))


def check_locations(node: ast.AST):
    """Implements the same checks as the VALIDATE_POSITIONS checks from Python/ast.c (Python 3.12.3)"""
    for attr in node._attributes:
        assert hasattr(node, attr)

    if "end_lineno" in node._attributes:
        assert node.end_lineno >= node.lineno
        if node.lineno < 0:
            assert node.end_lineno == node.lineno

    if "end_col_offset" in node._attributes:
        if node.col_offset < 0:
            assert node.end_col_offset == node.col_offset

    if "end_lineno" in node._attributes and "end_col_offset" in node._attributes:
        if node.lineno == node.end_lineno:
            assert node.end_col_offset >= node.col_offset

    for child in ast.iter_child_nodes(node):
        check_locations(child)


def test_if():
    source = """
        if x == 0:
            x += 2

        x += 3
    """
    import inspect

    source = inspect.cleandoc(source)
    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(1, 2), (1, 4)] == get_branches(code)


def test_if_else():
    import inspect

    source = """
        if x == 0:
            x += 1

        else:

            x += 2

        x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(1, 2), (1, 6)] == get_branches(code)


def test_if_elif_else():
    import inspect

    source = """
        if x == 0:
            x += 1
        elif x == 1:
            x += 2
        else:
            x += 3

        x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(1, 2), (1, 3), (3, 4), (3, 6)] == get_branches(code)


def test_if_nothing_after_it():
    import inspect

    source = """
        if x == 0:
            x += 1

    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(1, 0), (1, 2)] == get_branches(code)


def test_if_nested():
    import inspect

    source = """
        if x >= 0:
            y = 1
            if x > 1:
                if x > 2:
                    y = 2
        else:
            y = 4
            if x < 1:
                y = 3
            y += 10
        z = 0
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [
        (1, 2),
        (1, 7),
        (3, 4),
        (3, 11),
        (4, 5),
        (4, 11),
        (8, 9),
        (8, 10),
    ] == get_branches(code)


def test_if_in_function():
    import inspect

    source = """
        def foo(x):
            if x >= 0:
                return 1

        async def bar(x):
            if x == 0:
                return 1

        class Foo:
            def __init__(self, x):
                if x == 0:
                    self.x = 0

        foo(-1)
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 0), (2, 3), (6, 0), (6, 7), (11, 0), (11, 12)] == get_branches(code)


def test_keep_docstrings():
    import inspect

    source = """
        def foo(x):
            \"\"\"foo something\"\"\"
            if x >= 0:
                return 1

        async def bar(x):
            \"\"\"bar something\"\"\"
            if x == 0:
                return 1

        class Foo:
            \"\"\"Foo something\"\"\"
            def __init__(self, x):
                if x == 0:
                    self.x = 0

        foo(-1)
    """
    source = inspect.cleandoc(source)
    #    print(ast.dump(t, indent=True))

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(3, 0), (3, 4), (8, 0), (8, 9), (14, 0), (14, 15)] == get_branches(code)

    g = {}
    exec(code, g, g)

    assert "foo something" == g["foo"].__doc__
    assert "bar something" == g["bar"].__doc__
    assert "Foo something" == g["Foo"].__doc__


def test_branch_bad_positions():
    import inspect

    source = """\
def foo(x):
    if x == 0:
        t = '''\\
x == 0
'''
    else:
        t = '''\\
x != 0
'''
"""
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    # on Python 3.12.3, compile() raises a ValueError due to failing
    # the VALIDATE_POSITIONS checks from Python/ast.c
    compile(t, "foo.py", "exec")
    check_locations(t)


def test_for():
    import inspect

    source = """
        for v in [1, 2]:
            if v > 0:
                x += v

        x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(1, 2), (1, 5), (2, 1), (2, 3)] == get_branches(code)


def test_async_for():
    import inspect

    source = """
        import asyncio

        async def fun():
            global x

            async def g():
                yield 1
                yield 2

            async for v in g(): #10
                if v > 0:
                    x += v
            x += 3

        asyncio.run(fun())
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(10, 11), (10, 13), (11, 12), (11, 13)] == get_branches(code)


def test_for_else():
    import inspect

    source = """
        for v in [1, 2]:
            if v > 0:
                x += v
        else:
            x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(1, 2), (1, 5), (2, 1), (2, 3)] == get_branches(code)


def test_for_break_else():
    import inspect

    source = """
        for v in [1, 2]:
            if v > 0:
                x += v
            break
        else:
            x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(1, 2), (1, 6), (2, 3), (2, 4)] == get_branches(code)


def test_while():
    import inspect

    source = """
        v = 2
        while v > 0:
            v -= 1
            if v > 0:
                x += v

        x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 3), (2, 7), (4, 2), (4, 5)] == get_branches(code)


def test_while_else():
    import inspect

    source = """
        v = 2
        while v > 0:
            v -= 1
            if v > 0:
                x += v
        else:
            x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 3), (2, 7), (4, 2), (4, 5)] == get_branches(code)


def test_while_break_else():
    import inspect

    source = """
        v = 2
        while v > 0:
            v -= 1
            if v > 0:
                x += v
            break
        else:
            x += 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 3), (2, 8), (4, 5), (4, 6)] == get_branches(code)


def test_match():
    import inspect

    source = """
        v = 2
        match v:
            case 1:
                x = 1
            case 2:
                x = 2
        x += 2
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 4), (2, 6), (2, 7)] == get_branches(code)


def test_match_case_with_false_guard():
    import inspect

    source = """
        x = 0
        v = 1
        match v:
            case 1 if x > 0:
                x = 1
            case 1:
                x = 2
        x += 2
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(3, 5), (3, 7), (3, 8)] == get_branches(code)


def test_match_case_with_guard_isnt_wildcard():
    import inspect

    source = """
        def fun(v):
            match v:
                case _ if v > 0:
                    print("not default")
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 0), (2, 4)] == get_branches(code)


def test_match_branch_to_exit():
    import inspect

    source = """
        v = 5
        match v:
            case 1:
                x = 1
            case 2:
                x = 2
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 0), (2, 4), (2, 6)] == get_branches(code)


def test_match_default():
    import inspect

    source = """
        v = 5
        match v:
            case 1:
                x = 1
            case 2:
                x = 2
            case _:
                x = 3
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 4), (2, 6), (2, 8)] == get_branches(code)


def test_branch_after_case():
    import inspect

    source = """
        v = 1
        match v:
            case 1:
                if x < 0:  #4
                    x = 1
            case 2:
                if x < 0:  #7
                    x = 1
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 0), (2, 4), (2, 7), (4, 0), (4, 5), (7, 0), (7, 8)] == get_branches(
        code
    )


def test_branch_after_case_with_default():
    import inspect

    source = """
        v = 1
        match v:
            case 1:
                if x < 0:  #4
                    x = 1
            case 2:
                if x < 0:  #7
                    x = 1
            case _:
                if x < 0:  #10
                    x = 1
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [
        (2, 4),
        (2, 7),
        (2, 10),
        (4, 0),
        (4, 5),
        (7, 0),
        (7, 8),
        (10, 0),
        (10, 11),
    ] == get_branches(code)


def test_branch_after_case_with_next():
    import inspect

    source = """
        v = 1
        match v:
            case 1:
                if x < 0:  #4
                    x = 1
            case 2:
                if x < 0:  #7
                    x = 1
        x += 1
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 4), (2, 7), (2, 9), (4, 5), (4, 9), (7, 8), (7, 9)] == get_branches(
        code
    )


def test_match_wildcard_in_match_or():
    # Thanks to Ned Batchelder for this test case
    import inspect

    source = """
            def absurd(x):
                match x:
                    case (3 | 99 | (999 | _)):
                        print("default")
            absurd(5)
            """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 4)] == get_branches(code)


def test_match_capture():
    import inspect

    source = """
            def capture(x):
                match x:
                    case y:
                        print("default")
            capture(5)
            """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(2, 4)] == get_branches(code)


@pytest.mark.parametrize("star", ["", "*"])
def test_try_except(star):
    import inspect

    source = f"""
        def foo(x):
            try:
                y = x + 1
                if y < 0:
                    y = 0
            except{star} RuntimeException:
                if y < 2:
                    y = 0
            except{star} FileNotFoundError:
                if y < 2:
                    y = 0

            return 2*y
    """

    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(4, 5), (4, 13), (7, 8), (7, 13), (10, 11), (10, 13)] == get_branches(code)


def test_try_finally():
    import inspect

    source = """
        def foo(x):
            try:
                y = x + 1
                if y < 0:
                    y = 0
            finally:
                y = 2*y
    """
    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(4, 5), (4, 7)] == get_branches(code)


@pytest.mark.parametrize("star", ["", "*"])
def test_try_else(star):
    import inspect

    source = f"""
        def foo(x):
            try:
                y = x + 1
                if y < 0:
                    y = 0
            except{star} RuntimeException:
                if y < 2:
                    y = -1
            else:
                y = 2*y
    """

    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(4, 5), (4, 10), (7, 0), (7, 8)] == get_branches(code)


@pytest.mark.parametrize("star", ["", "*"])
def test_try_else_finally(star):
    import inspect

    source = f"""
        def foo(x):
            try:
                y = x + 1
                if y < 0:
                    y = 0
            except{star} RuntimeException:
                if y < 2:
                    y = -1
            else:
                if y > 5:
                    y = 42
            finally:
                y = 2*y
    """

    source = inspect.cleandoc(source)

    t = br.preinstrument(source)
    check_locations(t)
    code = compile(t, "foo", "exec")
    assert [(4, 5), (4, 10), (7, 8), (7, 13), (10, 11), (10, 13)] == get_branches(code)
