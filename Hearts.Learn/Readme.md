## Linux instructions

Install .NET:

    > sudo add-apt-repository ppa:dotnet/backports
    > sudo apt-get update && sudo apt-get install -y dotnet-sdk-10.0

Clone repositories:

    > git clone --recurse-submodules https://github.com/brianberns/Hearts.git

Build and run:

    > cd ./Hearts.Learn
    > dotnet run --configuration Release

TensorBoard:

    > apt install python3.10-venv (if necessary)
    > python3 -m venv venv
    > source venv/bin/activate
    > pip install tensorboard
    > tensorboard --bind_all --logdir ./Hearts.Learn/runs/

## Windows

TensorBoard:

    > cd .\source\repos\Hearts\
    > .\venv\Scripts\activate
    > tensorboard --logdir .\Hearts.DeepCfr\bin\Release\net8.0\
