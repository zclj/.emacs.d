
# Table of Contents

1.  [My Emacs config and documentation](#doc)
    1.  [Emacs](#emacs)
        1.  [Basics](#basics)
        2.  [Finding](#finding)
        3.  [Jump](#jump)
        4.  [Folding](#folding)
        5.  [Packages](#packages)
    2.  [Org-mode](#org-mode)
        1.  [Structure Editing](#structure-editing)
        2.  [Plain Lists](#plain-lists)
        3.  [Links](#links)
        4.  [Export](#export)
    3.  [Paredit](#paredit)
    4.  [Magit](#magit)
    5.  [Resources](#resources)


<a id="doc"></a>

# My Emacs config and documentation


<a id="emacs"></a>

## Emacs


<a id="basics"></a>

### Basics

-   Check the values of variables using `C-h v`


<a id="finding"></a>

### Finding

-   `M-x occur` Show definitions
-   `M-x imenu`


<a id="jump"></a>

### Jump

-   `C-x C-Space` (accross buffers)
-   `C-u C-Space` (buffer only)


<a id="folding"></a>

### Folding

-   Hideshow


<a id="packages"></a>

### Packages

-   `M-x list-packages`
    -   Use U to mark all upgradeable packages and x to perform upgrades


<a id="org-mode"></a>

## Org-mode


<a id="structure-editing"></a>

### [Structure Editing](https://orgmode.org/org.html#Structure-Editing)

-   `M-RET` (org-meta-return) Insert a new heading, item or row
-   `C-RET` (org-insert-heading-respect-content) Insert a new heading at the end of the current subtree
-   `M-UP` (org-move-subtree-up) Move subtree up
-   `M-DOWN` (org-move-subtree-down) Move subtree down
-   `M-S-LEFT/RIGHT` (org-promote/demote-subtree) Promote/demote the current subtree
-   `C-c *` (org-toggle-heading) Turn a normal line or plain list item into a headline


<a id="plain-lists"></a>

### Plain Lists

-   `M-S-RET` Insert a new item with checkbox
-   `S-UP/DOWN` Jump to the privous/next item in the current list
-   `C-c C-c` Toggle checkbox


<a id="links"></a>

### Links

-   [Handling links](https://orgmode.org/manual/Handling-links.html)
-   `C-c l` (org-store-link) Store a link to the current location.
-   `C-c C-l` (org-insert-link) When the cursor is on an existing link, link will be edited.
-   `C-c C-o` (org-open-at-point)


<a id="export"></a>

### Export

1.  Markdown

    -   `M-x org-md-export-to-markdown`
        -   ref: [Markdown export](https://orgmode.org/manual/Markdown-export.html)
    -   For inline code, enclose with = or ~


<a id="paredit"></a>

## Paredit

-   <http://danmidwood.com/content/2014/11/21/animated-paredit.html>


<a id="magit"></a>

## Magit


<a id="resources"></a>

## Resources

-   <https://github.com/jaypei/emacs-neotree>
-   <http://ergoemacs.org/emacs/effective_emacs.html>
-   <http://pragmaticemacs.com/emacs/join-line-to-previous-line/>
-   <https://github.com/bbatsov/super-save>
-   <http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/>

