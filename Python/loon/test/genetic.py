from functools import singledispatch


@singledispatch
def fprint(data):
    print(f'({type(data).__name__}) {data}')

@fprint.register(int)
@fprint.register(float)
def _(data):
    data = str(data)
    fprint(data)



import functools
@functools.singledispatch
def f(x):
    pass 
f()
