### 1
```bash
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install snap git nano curl
```


### 1.a As an optional step 
```bash
sudo adduser anyNameYouLike
sudo usermod -aG sudo anyNameYouLike
```
Creating a different user with sudo capabilities is an optional step, recommended for security in production environments. Close session and enter with the new created user

I have the node running,
I also checkout to 6aff97d596ac9d59460aab5c65627b1c8c0a1528
Then run nix-shell and is running for the last few minutes

### 2 Install Nix
```bash
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```

Then Log Off user and LogIn again.

Open a new terminal and verify the installations

```bash
nix --version
git --version
nano --version
```

### 3. IOHK Binary cache
Necessary to save installation time
```bash
sudo mkdir -p /etc/nix
sudo nano /etc/nix/nix.conf
```

include the following text into `nix.conf` file

```bash
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
experimental-features = nix-command flakes
```

### 4. PLUTUS-APPLICATION-FRAMEWORK
Go to your cardano development folder 

```bash
git clone https://github.com/input-output-hk/plutus-apps.git
```

### 5. Run Plutus Playground Server

```bash
cd plutus-apps
nix-shell
cabal update
```

```bash
cd plutus-playground-server
cabal update
plutus-playground-server
```

### 6. Run Plutus Playground Client
```bash
cd plutus-apps
nix-shell
```

```bash
cd plutus-playground-client
cabal update
npm install
npm start
```

