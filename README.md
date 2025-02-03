# AKT süvenduspraktikum 2025

[AKT süvenduspraktikumi 2025](https://courses.cs.ut.ee/2025/AKTSP/spring) materjalide repositoorium.


## Paigaldamine
1. Sõltuvalt operatsioonisüsteemist:
    * Windows: paigalda [WSL](https://docs.microsoft.com/en-us/windows/wsl/install). **Edasi tööta WSL-is.**
        * Paigalda opam-i ja OCaml-i jaoks vajalikud paketid: `sudo apt install bubblewrap unzip bzip2 gcc make`
    * Linux: ära tee midagi, tegid juba õige valiku!
    * MacOS: paigalda [homebrew](https://brew.sh/).
2. Paigalda [opam](https://opam.ocaml.org/doc/Install.html).
3. Paigalda [Visual Studio Code](https://code.visualstudio.com/) ja [OCaml Platform laiendus](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform).
    * Kui kasutad WSL-i, siis paigalda ka [WSL laiendus](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl).
4. [Git](https://git-scm.com/downloads)-iga klooni [see repositoorium](https://github.com/sws-lab/aktsp2025).
    * NB! Windows-is klooni repositoorium WSL-i sisse (nt sealsesse kodukausta `~`), mitte väljaspoole (`/mnt/c/...`), sest muidu toimub OCaml-i kompileerimine väga aeglaselt.
5. Navigeeri käsureal kloonitud repositooriumi kausta.
6. Käivita: `opam init -a --bare -y`
7. Käivita: `opam switch create --deps-only --locked -y . 5.3.0` (NB! punkt)

### Paigalduse kontrollimine
1. Käivita: `eval $(opam env)`
2. Käivita: `dune exec src/hello/hello.exe`
    * Väljund peaks olema `Hello, OCaml!`.


## Kasutamine
1. Navigeeri käsureal kloonitud projekti repositooriumi kausta.
2. Käivita: `eval $(opam env)`
3. Käivita: `code .` (NB! punkt)
4. Käivita
    * Kompileerimiseks: `dune build`
    * Jooksvalt testimiseks: `dune runtest -w`
    * Jooksvalt osaliseks testimiseks: `dune runtest -w test/crashcourse/`
    * Interaktiivselt kasutamiseks: `dune utop`
