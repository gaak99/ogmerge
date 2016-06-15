# Ogmerge (Oh Gee merge)
An Emacs pkg to merge changes in shared Emacs/Orgzly Org notes. 

It's possible that Emacs and Orgzly mobile app can modify the same Org note shared via $cloud. Currently there is no good way to resolve those changes. _git_ is a good solution and on the Orgzly todo list. Until that feature is avail here is a wrapper around Emacs Ediff to merge changes manually and keep the shared note file in sync. 

## Keywords
emacs ediff org-mode orgzly dropbox cloud sync

## Install
(later will be avail on MELPA if all goes well)

	;; dependancies: make sure f & s & dash pkgs installed
    (add-to-list 'load-path "/path/to/ogmerge")
    (require 'ogmerge)

## User setq-able Emacs init vars
The first two need to be looked at closely and set correctly in your Emacs init file. The rest of the defaults are probably fine.

```lisp
(defvar ogmerge-local-dir (f-canonical org-directory)
  "Local org dir aka Emacs org dir.")
(defvar ogmerge-remote-dir (f-canonical org-orgzly-directory)
  "Remote org dir aka upstream in cloud (aka Orgzly Save path in Dropbox).")

(defvar ogmerge-backup? t
  "Pre destructive op, make an emergency backup copy in subdir ogmerge-backup-dir.")
(defvar ogmerge-backup-dir ".ogmerge-bkup"
  "Name of subdir for emergency copies.")

(defvar ogmerge-verbose t
  "Get more output to *messages*.")
```

## Shared note merge flow (typically once a day if a shared note is modified)
### One Emacs instance and one Orgzly instance (Android device)
*WARNING* - this is immature software and thus needs more real world testing so make sure you have backups of any data files you can't afford to lose (at least until you are convinced yourself it is solid or at least the emergency backup copies it makes are solid (see init var _ogmerge-backup?_ and _ogmerge-backup-dir_)).

1. Orgzly _Save_ (long-press Forced if needbe) note to $cloud
2. Emacs run _ogmerge-ediff_ (and answer filename prompt with shared org note (base) filename)
   * ediff merge/resolve changed chunks
   * ediff save/overwrite (_save-buffer_) merged buf to same local file (answer 'y' to prompts to overwrite)
   * ediff quit
3. Emacs _ogmerge-push_ local file to $cloud (orgzly) dir (and answer filename prompt with shared org note (base) filename)
4. Orgzly _Load_ note

### One Emacs instance and multiple Orgzly instances (Android devices)

1. Repeat above for each Orgzly instance
2. After all of them done, Orgzly _Load_ all instances (except last one synced in step 1)

## Notes
* ediff skillz def a plus here. But if not currently avail then this is good way to learn it. It's def a non-trivial -- UI-wise and concept-wise  -- Emacs app.
* Typically in ediff you'll choose file A or file B for each change chunk, but for this type of merge (2 way) sometimes (appended chunks in A&B for example) you may want both and thus you should hand edit the merge buf (better way?).
* Orgzly likes adding blank lines so don't ediff merge them out on Emacs else u will keep seeing them come back -- zombielike --  to haunt you and must re-merge again and again.
* BTW if you don't dig your ediff config try mines

```lisp
;; don't start another frame
;; this is done by default in preluse
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;; revert windows on exit - needs winner mode
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(add-hook 'ediff-prepare-buffer-hook #'show-all)
```

## Test/status (pre beta)
* Only tested with Dropbox as $cloud so far
* Only real world tested for 8 weeks by 1 person (need users/testers!)
* Only tested with shared file between one Emacs and 1&2 Orgzly Android devices

### Unit tests
```bash
CASK_EMACS=/usr/local/Cellar/emacs/24.5/bin/emacs cask emacs --batch  --script test/ogmerge-test.el
ogmerge-test-ediff ... PASS.
ogmerge-test-push ... PASS.
ogmerge-test-pull ... PASS.
ogmerge-test-savecopy ... PASS.
```

#### Cask notes:
* Mac OS Homebrew installed cask 0.7.4 seems not to have latest fixes from github bundled so if _cask emacs_ fails, try swapping in bin/cask from github (works for /me:-)

## Todo
* should offer -- when possible -- auto merge option? Don't think it's possible w/2way merge. See Live-to-merge link below.

## License
Ogmerge is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
later version.

Ogmerge is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

## Refs
<http://www.orgzly.com>

<https://www.gnu.org/software/emacs/manual/html_node/ediff/>

<http://blog.plasticscm.com/2010/11/live-to-merge-merge-to-live.html?m=1>
