import abc

class Type:

    @abc.abstractmethod
    def len_words(self):
        pass


class Struct(Type):
    def __init__(self, fields):
        self.fields = {}
        offset = 0
        for name in fields:
            self.fields[name] = offset
            offset += 4

    def get_field_offset(self, name):
        return self.fields[name]

    def len_words(self):
        return len(self.fields)
