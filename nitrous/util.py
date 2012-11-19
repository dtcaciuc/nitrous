def dump_ast(node, annotate_fields=True, include_attributes=False):
    import ast

    INCR = '    '

    def _format(node, indent=''):

        if isinstance(node, ast.AST):
            fields = [(a, _format(b, indent + INCR))
                      for a, b in ast.iter_fields(node)]
            rv = '%s(\n%s' % (node.__class__.__name__, ',\n'.join(
                ((indent + '%s=%s') % field for field in fields)
                if annotate_fields else
                (b for a, b in fields)
            ))
            if include_attributes and node._attributes:
                rv += fields and ',\n' or ' '
                rv += ',\n'.join(indent + '%s=%s' % (
                    a, _format(getattr(node, a), indent + '  ')
                ) for a in node._attributes)
            if rv.endswith("(\n"):
                rv = rv[:-1]
            return rv + ')'

        elif isinstance(node, list):
            inner = ',\n'.join(indent + _format(x, indent + INCR)
                               for x in node)
            if inner:
                inner = '\n' + inner + '\n' + indent

            return '[%s]' % inner

        return repr(node)

    if not isinstance(node, ast.AST):
        raise TypeError('expected AST, got %r' % node.__class__.__name__)

    return _format(node, INCR)
