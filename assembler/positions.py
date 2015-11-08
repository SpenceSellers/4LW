
def uniqueID():
    return str(uuid.uuid4())[:8]

class Position:
    def is_absolute(self):
        return False

    def is_scoped(self):
        return True

    def is_label(self, s):
        return False

class LabelPos(Position):
    def __init__(self, name):
        self.name = name

    def is_label(self, s):
        return s == self.name

    def __repr__(self):
        return "L{}".format(self.name)

class AbsoluteLabelPos(LabelPos):
    def __init__(self, name):
        super().__init__(name)

    def is_absolute(self):
        return True

class GlobalLabelPos(LabelPos):
    def __init__(self, name):
        super().__init__(name)

    def __repr__(self):
        return "#{}".format(self.name)

    def is_scoped(self):
        return False


class EnscopedPos(Position):
    def __init__(self, scopeid, inner):
        self.scopeid = scopeid
        self.inner = inner

    def descope(self):
        return self.inner

    def is_label(self, s):
        # TODO: This is likely wrong. What if it's the same name in different scopes??
        return self.inner.is_label(s)

    def in_scope(self, scopeid):
        return scopeid == self.scopeid

    def __repr__(self):
        return "{}/{}".format(self.scopeid, self.inner)

class PointingPos(Position):
    def __init__(self, to_name):
        self.to_name = to_name

    def __repr__(self):
        return "point:{}".format(self.to_name)

    def is_scoped(self):
        return False
