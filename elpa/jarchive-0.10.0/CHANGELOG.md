# Change Log

## 2023-01-18 0.10.0 Release Notes
- Thank you Robert Brown for the following contributions:
  - Handle opening archives with the .zip extension
  - Handle opening files (within archives) that do not have a `.` in the name 
  - Some cleanup in the README

## 2022-12-07 0.9.0 Release Notes
- Add missing file-name-handler operations
  - Thank you Michael Albinus for pointing this out on the bug-gnu-emacs mailing list!

## 2022-11-14 0.8.0 Release Notes
- Readme Changes

## 2022-11-13 0.7.0 Release Notes
- Add info about jarchive on ELPA

## 2022-11-13 0.6.0 Release Notes
- Add link to bug tracker

## 2022-11-13 0.5.0 Release Notes
- Fix Typo

## 2022-11-13 0.4.0 Release Notes
- Remove some unnecessary messages
- Misc non breaking fixes
- Quiet down eglot patch messages in case it's called from a hook

## 2022-11-12 0.3.0 Release Notes

- Assigned copyright to Free Software Foundation (elpa requirement)
- Bumped version in jarchive.el to 0.3.0 (forgot to bump to 0.2.0 in last version)

## 2022-11-12 0.2.0 Release Notes
- Documentation updates in preparation for submitting to elpa or melpa
- BREAKING: Patching legacy Eglot no longer happens automatically when calling `jarchive-setup`.
  - Users can call `(with-eval-after-load 'eglot (jarchive-patch-eglot))` now in their config instead.
- `jarchive-patch-eglot` attempts to print warnings when it is called at the wrong time.

## 2022-11-11 0.1.0 Release Notes

- Now operates on full jar URIs and zipfile URIs
  - For example `jar:file:///path/to/library.jar!/path/in/jar/source.ext`
  - also `zipfile:///path/to/library.jar::/path/in/jar/source.ext`
- Removed command `jarchive-move-to-visiting-project`
  - Prefer setting `eglot-extend-to-xref` instead, or use `M-x write-file` to save where you like.
- Removed `jarchive--managed-mode`
  - No longer necessary now that `eglot-extend-to-xref` is working properly.
- Works with the latest eglot on emacs master
  - as of commit [1a2d603bb3938ff68ed1a5412d131b41efd40a24](https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=1a2d603bb3938ff68ed1a5412d131b41efd40a24 "Emacs upstream commit 1a2d603bb3938ff68ed1a5412d131b41efd40a24").
- Patches the legacy eglot (version 1.9, released 2022-10-11) on melpa that does not contain the changes from above commit to emacs mainline
  - Currently what is available to stable emacs users (version 28.x)
  - https://elpa.gnu.org/packages/eglot.html 
