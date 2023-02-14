def b(b):
    return b

def lam(f):
    return lambda x : f(x)

def app(f,x):
    return f(x)



print(app(lam(lambda x: x), b(True)))