
# Table of Contents

1.  [My Emacs config and documentation](#orge8668d4)
    1.  [Emacs](#orgf41c6fb)
        1.  [Basics](#org8c42ff9)
        2.  [Finding](#org4428d2b)
        3.  [Jump back](#org7259f44)
        4.  [Folding](#org388fb53)
        5.  [Packages](#org2f98a04)
    2.  [Org-mode](#orgbefebc3)
        1.  [Structure Editing](#org8b33741)
        2.  [Links](#org997b3a1)
        3.  [Export](#org3ca6e90)
    3.  [Paredit](#org17b9535)
    4.  [Magit](#org730abaa)
    5.  [Resources](#orgd5a7420)


<a id="orge8668d4"></a>

# My Emacs config and documentation


<a id="orgf41c6fb"></a>

## Emacs


<a id="org8c42ff9"></a>

### Basics

-   Check the values of variables using `C-h v`


<a id="org4428d2b"></a>

### Finding

-   `M-x occur` Show definitions
-   `M-x imenu`


<a id="org7259f44"></a>

### Jump back

-   `C-x C-Space` (accross buffers)
-   `C-u C-Space` (buffer only)


<a id="org388fb53"></a>

### Folding

-   Hideshow


<a id="org2f98a04"></a>

### Packages

-   `M-x list-packages`
    -   Use U to mark all upgradeable packages and x to perform upgrades


<a id="orgbefebc3"></a>

## Org-mode


<a id="org8b33741"></a>

### [Structure Editing](https://orgmode.org/org.html#Structure-Editing)

-   `M-UP` (org-move-subtree-up) Move subtree up
-   `M-DOWN` (org-move-subtree-down) Move subtree down


<a id="org997b3a1"></a>

### Links

-   [Handling links](https://orgmode.org/manual/Handling-links.html)
-   `C-c l` (org-store-link) Store a link to the current location.
-   `C-c C-l` (org-insert-link) When the cursor is on an existing link, link will be edited.
-   `C-c C-o` (org-open-at-point)


<a id="org3ca6e90"></a>

### Export

1.  Markdown

    -   `M-x org-md-export-to-markdown`
        -   ref: [Markdown export](https://orgmode.org/manual/Markdown-export.html)
    -   For inline code, enclose with = or ~


<a id="org17b9535"></a>

## Paredit

-   <http://danmidwood.com/content/2014/11/21/animated-paredit.html>


<a id="org730abaa"></a>

## Magit


<a id="orgd5a7420"></a>

## Resources

-   <https://github.com/jaypei/emacs-neotree>
-   <http://ergoemacs.org/emacs/effective_emacs.html>
-   <http://pragmaticemacs.com/emacs/join-line-to-previous-line/>
-   <https://github.com/bbatsov/super-save>
-   <http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/>

