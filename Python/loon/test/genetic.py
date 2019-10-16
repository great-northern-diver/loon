from functools import singledispatch

class class1:
    def __init__(self, x):
        self.x = x

class class2:
    def __init__(self, x):
        self.x = x + 2

x = class1(1)
y = class2(1)

@singledispatch
def fprint(data):
    print(f'({type(data).__name__}) {data}')

@fprint.register(class1)
def _(data):
    print(data.x)



import functools
@functools.singledispatch
def f(x):
    pass 
f()
