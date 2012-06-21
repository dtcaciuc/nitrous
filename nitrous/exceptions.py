class AnnotationError(Exception):
    """Raised on issues during function annotation (eg. signature declaration etc)."""
    pass


class TranslationError(Exception):
    """Raised on issues during Python-to-IR translation."""
    pass
