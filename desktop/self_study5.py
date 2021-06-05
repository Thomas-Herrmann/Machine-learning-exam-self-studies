import torch
from pathlib import Path
import requests
from matplotlib import pyplot
import matplotlib.pyplot as plt
import numpy as np
import pickle
import gzip


DATA_PATH = Path("data")
PATH = DATA_PATH / "mnist"

PATH.mkdir(parents=True, exist_ok=True)

URL = "http://deeplearning.net/data/mnist/"
FILENAME = "mnist.pkl.gz"

if not (PATH / FILENAME).exists():
        content = requests.get(URL + FILENAME).content
        (PATH / FILENAME).open("wb").write(content)


with gzip.open((PATH / FILENAME).as_posix(), "rb") as f:
    ((x_train, y_train), (x_valid, y_valid), (x_test, y_test)) = pickle.load(f, encoding="latin-1")

    pyplot.imshow(x_train[0].reshape((28, 28)), cmap="gray")
    print(x_train.shape)

    x_train, y_train, x_valid, y_valid, x_test, y_test = map(
        torch.tensor, (x_train, y_train, x_valid, y_valid, x_test, y_test)
    )
    n, c = x_train.shape
    x_train, x_train.shape, y_train.min(), y_train.max()
    print(f"Training data (images): \n {x_train}")
    print(f"Training data (labels): \n {y_train}")
    print(f"Shape of x_train (now torch tensor) holding the training images: {x_train.shape}")
    print(f"Min and max label values: {y_train.min()}, {y_train.max()}")

    weights = torch.randn(784, 10) / np.sqrt(784)
    weights.requires_grad_()
    bias = torch.zeros(10, requires_grad=True)

    def softmax(x):
        return x.exp() / x.exp().sum(-1).unsqueeze(-1)

    # Below @ refers to matrix multiplication
    def model(xb):
        return softmax(xb @ weights + bias)

    batch_size = 64
    xb = x_train[0:batch_size]
    print(f"Batch shape: {xb.shape}")
    preds = model(xb)
    print(f"Prediction on first image {preds[0]}")
    print(f"Corresponding classification: {preds[0].argmax()}")

    def nll(input, target):
        return (-input[range(target.shape[0]), target].log()).mean()

    loss_func = nll

    # Make a test calculation
    yb = y_train[0:batch_size]
    print(loss_func(preds,yb))

    def accuracy(out, yb):
        preds = torch.argmax(out, dim=1)
        return (preds == yb).float().mean()

    print(f"Accuracy of model on batch (with random weights): {accuracy(preds, yb)}")

    epochs = 10  # how many epochs to train for
    lr = 0.01  # learning rate

    # We recorded the losses in lists for later plotting
    train_losses = []
    valid_losses = []

        # Iterate for a fixed number of epochs. One epoch is an iteration of the data set, read in chucks of size batch_size
    for epoch in range(epochs):
        for batch_idx in range((n - 1) // batch_size + 1):

            # pick out the relevant batch
            start_i = batch_idx * batch_size
            end_i = start_i + batch_size
            xb = x_train[start_i:end_i]
            yb = y_train[start_i:end_i]

            # Do prediction for all elements in the batch
            pred = model(xb)
            # and calculate the loss
            loss = loss_func(pred, yb)

            # Do back propagation to find the gradients
            loss.backward()
            with torch.no_grad():
                # Update the weights
                weights -= weights.grad * lr
                bias -= bias.grad * lr
                weights.grad.zero_()
                bias.grad.zero_()

                if batch_idx % 100 == 0:
                    with torch.no_grad():
                        train_loss = loss_func(model(x_train), y_train)
                        print(f"Epoch: {epoch}, B-idx: {batch_idx}, Training loss: {train_loss}")
                        train_losses.append(train_loss)
    
    plt.plot(range(len(train_losses)), train_losses,'b')

