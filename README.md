Miei file di configurazione per GNU Emacs.  *Non conosco l'Emacs Lisp*, questi
file sono un aggregato di codici trovati in rete, puoi utilizzarli ma **a tuo
rischio e pericolo**.

Nel file di inizializzazione `~/.emacs` inserisci
````Lisp
(load "<PERCORSO>/dotemacs")
(load "<PERCORSO>/dotemacs-fortran")
(load "<PERCORSO>/dotemacs-latex")
(load "<PERCORSO>/dotemacs-laptop")
(load "<PERCORSO>/dotemacs-org")
````
per caricare i file di configurazione desiderati.

Imposta Emacs come editor di testo per i commit di git con il comando
````shell
git config --global core.editor "emacs -Q -l <PERCORSO>/dotemacs-git-commit.el"
````
In questo modo verrà caricato solo il file `dotemacs-git-commit.el` che richiede
la minor mode `git-commit-mode`.
