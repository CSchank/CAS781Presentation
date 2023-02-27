from abc import ABC, abstractmethod

# "Tagged" format for evaluation
class B:
    def __init__(self, b):
        self.b = b

class Lam:
    def __init__(self,f):
        self.f = f

class App:
    def __init__(self,f,x):
        self.f = f
        self.x = x

def eval(e):
    if isinstance(e, B):
        return e.b
    elif isinstance(e, Lam):
        return lambda x : e.f(x)
    elif isinstance(e, App):
        if isinstance(e.f,Lam):
            f = eval(e.f)
            return f(eval(e.x))

print(eval(App(Lam(lambda x: x), B(True))))

# Tagless format for evaluation
def b(b):
    return b

def lam(f):
    return lambda x : f(x)

def app(f,x):
    return f(x)

print(app(lam(lambda x: x), b(True)))


# More Generic Tagless format using abstract base class
class Sym(ABC):
    @abstractmethod
    def b(self,b): pass

    @abstractmethod 
    def lam(f): pass

    @abstractmethod
    def app(f,x): pass

class Eval(Sym):
    def b(self,b):
        return b
    
    def lam(self,f): 
        return lambda x : f(x)

    def app(self,f,x):
        return f(x)

e = Eval()
print(e.app(e.lam(lambda x: x), e.b(True)))

class CC(Sym):
    def b(self,b):
        return 1
    
    def lam(self,f): 
        return 1 + f(0)

    def app(self,f,x):
        return 1 + f + x

c = CC()
test = c.app(c.lam(lambda x: x), c.b(True))
print(test)

def b(b):
    return lambda c: c.b(b)

def lam(f):
    return lambda c: c.lam(f)

def app(f,x):
    return lambda c: c.app(f(c),x(c))
    
def run(f,c):
    return f(c)
        

test = app(lam(lambda x: x), b(True))
print(run(test,Eval()))
print(run(test,CC()))