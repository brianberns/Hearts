namespace Hearts.Model

module Settings =

    /// Size of a neural network hidden layer.
    let hiddenSize = Encoding.encodedLength * 8

    /// Device to use for training models.
    let device = TorchSharp.torch.CUDA
