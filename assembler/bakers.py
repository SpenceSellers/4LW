import asm2 as asm
import string
import copy
import sys
import uuid

def log(s):
    print(s, file=sys.stderr)

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
    def __init__(self, baker):
        self.scopeid = str(uuid.uuid4())
        self.inner = baker

    def render(self, table):
         # We have to restore the original names of our scoped items.

         # We just need the original names
        innertable = copy.copy(self.inner.report())
        union = copy.copy(table)

        for label, value in innertable.items():
             # Find the new name, get the value, and apply it to the old name.
            scopedname = self.enscope(label)
            union[label] = table[scopedname]

        return self.inner.render(union)

    def report(self):
         # We need the labels to propagate outwards so that global positions
         # can be calculated, but they shouldn't be able to be used.
        innertable = self.inner.report()
        scopedtable = {}
        for label, sub_pos in innertable.items():
             # Hide these labels behind the scope id.
            newlabel = self.enscope(label)
            scopedtable[newlabel] = sub_pos
        return scopedtable

    def length(self):
        return self.inner.length()

    def enscope(self, label):
        return str(self.scopeid) + ':' + str(label)


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
            return {self.label: 0}
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
        return {self.label: 0}

    def length(self):
        return 0

    def __repr__(self):
        return "LabelBaker {}".format(self.label)

class Pointing(Baker):
    def __init__(self, label):
        self.label = label

    def render(self, table):
        try:
            pointer = table[self.label]
        except:
            raise Exception("Unknown label in pointer: {}".format(self.label))
        result = asm.expandWord(asm.toBase27(pointer))
        if len(result) != 4:
            raise Exception("Pointing baker returns pointer that is too large: {}".format(result))
        return result

    def length(self):
        return 4
