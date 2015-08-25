import asm2 as asm

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

class Baked(Baker):
    def __init__(self, s, label = None):
        self.s = s
        self.label = label

    def render(self, table):
        return self.s

    def length(self):
        return len(self.s)

    def report(self):
        if self.label:
            return {self.label: 0}
        else:
            return {}
            
class LabelBaker(Baker):
    def __init__(self, label):
        self.label = label

    def render(self, table):
        return ''

    def report(self):
        return {self.label: 0}

    def length(self):
        return 0

class Pointing(Baker):
    def __init__(self, label):
        self.label = label

    def render(self, table):
        try:
            pointer = table[self.label]
        except:
            raise Exception("Unknown label in pointer: {}".format(self.label))
        result = asm.expandWord(asm.toBase27(pointer))
        return result

    def length(self):
        return 4
