import torch

sigmoid = torch.nn.Sigmoid()
relu = torch.nn.ReLU()


def lin_net(x=1.0, y=1.0, Nh=4):
    
    X = torch.tensor(x, requires_grad=True)
    Y = torch.tensor(y, requires_grad=True)

    prev = X

    for i in range(1, Nh + 1):

        W = torch.tensor(0.5, requires_grad=True)
        W.register_hook((lambda ic : lambda grad: print(f"Grad W_{ic} = {grad}"))(i))
        H = prev * W
        H.register_hook((lambda ic : lambda grad: print(f"Grad H_{ic} = {grad}"))(i))

        prev = H
    
    W_o = torch.tensor(0.5, requires_grad=True)
    W_o.register_hook(lambda grad: print(f"Grad W_o = {grad}"))

    O = prev * W_o
    O.register_hook(lambda grad: print(f"Grad O = {grad}"))

    return 2 * O - 2 * Y


e = lin_net(x=1.0, y=1.0, Nh=100)

e.backward()
