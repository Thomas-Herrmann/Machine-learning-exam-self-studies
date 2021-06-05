import torch

sigmoid = torch.nn.Sigmoid()
relu = torch.nn.ReLU()

x1 = torch.tensor(1.0, requires_grad=True)
w1 = torch.tensor(0.5, requires_grad=True)

x2 = torch.tensor(2.0, requires_grad=True)
w2 = torch.tensor(0.5, requires_grad=True)

w0 = torch.tensor(0.5, requires_grad=True)

t = torch.tensor(1.0, requires_grad=True)

y0 = x1*w1
y0.register_hook(lambda grad: print("Grad y0 = {}".format(grad)))

y1 = x2*w2
y1.register_hook(lambda grad: print("Grad y1 = {}".format(grad)))

y2 = y0+y1
y2.register_hook(lambda grad: print("Grad y2 = {}".format(grad)))

y3 = relu(y2)
y3.register_hook(lambda grad: print("Grad y3 = {}".format(grad)))

y4 = y3*w0
y4.register_hook(lambda grad: print("Grad y4 = {}".format(grad)))

o = sigmoid(y4)
o.register_hook(lambda grad: print("Grad o = {}".format(grad)))

e = t * torch.log(o) - (1.0 - t) * torch.log(o)

e.backward()

print("Grad w0 = {}".format(w0.grad))
print("Grad w1 = {}".format(w1.grad))
print("Grad w2 = {}".format(w2.grad))


print("Done")