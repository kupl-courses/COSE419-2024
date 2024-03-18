# Installing on Windows
1. Install Vagrant (https://developer.hashicorp.com/vagrant/install?product_intent=vagrant)
2. Install VirtualBox (https://www.oracle.com/kr/virtualization/technologies/vm/downloads/virtualbox-downloads.html)
3. Run 
```
vagrant up
vagrant ssh
```

# Installing on MacOS
1. Install homebrew (https://brew.sh)
2. Install opam (https://opam.ocaml.org/doc/Install.html)
```
brew install opam
opam init
eval $(opam env --switch=default)
```
3. Install dune, etc
```
opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
```
If dune is not foud, add the following in .bash_profile (or .zprofile)
```
eval $(opam config env) 
```
4. Install z3, etc
```
opam install mtime z3 batteries core
```
