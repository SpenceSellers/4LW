import asm2 as asm
import string
import copy
import sys
import uuid

from positions import *

def log(s):
    print(s, file=sys.stderr)

def uniqueID():
    return str(uuid.uuid4())[:8]

class Baker:
    def render(self, table):
        return ''

    def report(self):
        return {}

    def length(self):
        return 0

    def render_total(self):
        locs = self.report()
        return self.render(locs)

    def debug_labels(self):
        s = ''
        locs = self.report()
        for name, index in locs.items():
            s += "{}: {}\n".format(name, index)
        return s

class PositionShiftedBaker(Baker):
    def __init__(self, shift, internal):
        self.shift = shift
        self.internal = internal

    def render(self, table):
        return self.internal.render(table)

    def report(self):
        interior = self.internal.report()
        actual = {}
        for label, sub_pos in interior.items():
            actual[label] = sub_pos + self.shift

        return actual

    def __repr__(self):
        return "Position by {} of ({})".format(self.shift, self.internal)

class InjectAbsolute(Baker):
    def __init__(self, injected, internal):
        self.internal = internal
        self.injected = injected

    def render(self, table):
        return self.internal.render(table)

    def report(self):
        reported = copy.copy(self.internal.report())
        for label, pos in self.injected.items():
            reported[AbsoluteLabelPos(label)] = pos
        return reported

    def __repr__(self):
        return "Injected labels {} into ({})".format(self.injected, self.internal)

class BakerSequence(Baker):
    def __init__(self, seq):
        self.seq = seq

    def render(self, table):
        result = ''
        for baker in self.seq:
            chunk = baker.render(table)
            assert len(chunk) == baker.length()
            result += chunk
        return result

    def report(self):
        locs = {}
        idx = 0
        for baker in self.seq:
            new_locs = baker.report()
            for label, sub_pos in new_locs.items():
                locs[label] = idx + sub_pos
            idx += baker.length()
        return locs

    def length(self):
        c = 0
        for baker in self.seq:
            c += baker.length()
        return c

    def __repr__(self):
        return "BakerSequence: {}".format(self.seq)

class CaptureScopeBaker(Baker):
    def __init__(self, baker, pretty_name = None):
        self.scopeid = uniqueID()
        if pretty_name:
            self.scopeid = pretty_name + '_' + self.scopeid
        self.inner = baker

    def render(self, table):
         # We have to restore the original names of our scoped items.
         # We just need the original names
        union = copy.copy(table)

        for label, value in table.items():
            # Skip unscoped items.
            if not label.is_scoped(): continue

            # Find the new name, get the value, and apply it to the old name.
            if isinstance(label, EnscopedPos) and label.in_scope(self.scopeid):
                union[label.descope()] = table[label]

        return self.inner.render(union)

    def report(self):
         # We need the labels to propagate outwards so that global positions
         # can be calculated, but they shouldn't be able to be used.
        innertable = self.inner.report()
        scopedtable = {}
        for label, sub_pos in innertable.items():
            if not label.is_scoped():
                scopedtable[label] = sub_pos
            else:
                # Hide these labels behind the scope id.
                newlabel = EnscopedPos(self.scopeid, label)
                scopedtable[newlabel] = sub_pos

        return scopedtable

    def length(self):
        return self.inner.length()

    def enscope(self, label):
        return str(self.scopeid) + ':' + str(label)

    def __repr__(self):
        return "CaptureScopeBaker {} ({})".format(self.scopeid, self.inner)


class Baked(Baker):
    def __init__(self, s, label = None):
        self.s = s
        self.label = label
        self.validate()

    def render(self, table):
        return self.s

    def length(self):
        return len(self.s)

    def report(self):
        if self.label:
            return {LabelPos(self.label): 0}
        else:
            return {}

    def validate(self):
        for char in self.s:
            if char not in string.ascii_uppercase + '_':
                raise ValueError("{} is not a 4LW letter!".format(char))


    def __repr__(self):
        return "Baked {}: {}".format(self.label, self.s)

class LabelBaker(Baker):
    def __init__(self, label):
        self.label = label

    def render(self, table):
        return ''

    def report(self):
        return {LabelPos(self.label): 0}

    def length(self):
        return 0

    def __repr__(self):
        return "LabelBaker {}".format(self.label)

class Pointing(Baker):
    def __init__(self, label):
        self.label = label

    def render(self, table):
        pointing = self.get_pointing_to(table)
        pointer = table[pointing]

        result = asm.expandWord(asm.toBase27(pointer))
        if len(result) != 4:
            raise Exception("Pointing baker returns pointer that is too large: {}".format(result))
        return result

    def get_pointing_to(self, table):
        pointing = None
        for k, pos in table.items():
            if k.is_label(self.label):
                pointing = k

        if pointing == None:
            raise Exception("Unknown label in pointer: {}".format(self.label))

        return pointing

    def is_pointing_to_absolute(self, table):
        return self.get_pointing_to(table).is_absolute()

    def report(self):
        # We want to report that we exist, so that the assembler can report all pointer locations.
        return {PointingPos(self.label): 0}

    def length(self):
        # A pointer is one word long.
        return 4

    def __repr__(self):
        return "[Pointing to {}]".format(self.label)
