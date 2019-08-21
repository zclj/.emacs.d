
# Table of Contents

1.  [My Emacs config and documentation](#orgd660d4e)
    1.  [Emacs](#org9ed5ecc)
        1.  [Basics](#orgd80ccea)
        2.  [Finding](#org0dd4dd4)
        3.  [Jump back](#orgd47efdc)
        4.  [Folding](#orgfc9c067)
        5.  [Packages](#orga06706d)
    2.  [Org-mode](#org9d50fcd)
        1.  [Structure Editing](#orgf23643b)
        2.  [Links](#orgf0fdc32)
        3.  [Export](#org9535b7f)
    3.  [Paredit](#orgc2a5e38)
    4.  [Magit](#org0d47b6b)
    5.  [Resources](#orge4df01b)


<a id="orgd660d4e"></a>

# My Emacs config and documentation


<a id="org9ed5ecc"></a>

## Emacs


<a id="orgd80ccea"></a>

### Basics

-   Check the values of variables using `C-h v`


<a id="org0dd4dd4"></a>

### Finding

-   `M-x occur` Show definitions
-   `M-x imenu`


<a id="orgd47efdc"></a>

### Jump back

-   `C-x C-Space` (accross buffers)
-   `C-u C-Space` (buffer only)


<a id="orgfc9c067"></a>

### Folding

-   Hideshow


<a id="orga06706d"></a>

### Packages

-   `M-x list-packages`
    -   Use U to mark all upgradeable packages and x to perform upgrades


<a id="org9d50fcd"></a>

## Org-mode


<a id="orgf23643b"></a>

### [Structure Editing](https://orgmode.org/org.html#Structure-Editing)

-   `M-UP` (org-move-subtree-up) Move subtree up
-   `M-DOWN` (org-move-subtree-down) Move subtree down


<a id="orgf0fdc32"></a>

### Links

-   [Handling links](https://orgmode.org/manual/Handling-links.html)
-   `C-c l` (org-store-link) Store a link to the current location.
-   `C-c C-l` (org-insert-link) When the cursor is on an existing link, link will be edited.
-   `C-c C-o` (org-open-at-point)


<a id="org9535b7f"></a>

### Export

1.  Markdown

    -   `M-x org-md-export-to-markdown`
        -   ref: [Markdown export](https://orgmode.org/manual/Markdown-export.html)
    -   For inline code, enclose with = or ~


<a id="orgc2a5e38"></a>

## Paredit

-   <http://danmidwood.com/content/2014/11/21/animated-paredit.html>


<a id="org0d47b6b"></a>

## Magit


<a id="orge4df01b"></a>

## Resources

-   <https://github.com/jaypei/emacs-neotree>
-   <http://ergoemacs.org/emacs/effective_emacs.html>
-   <http://pragmaticemacs.com/emacs/join-line-to-previous-line/>
-   <https://github.com/bbatsov/super-save>
-   <http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/>

