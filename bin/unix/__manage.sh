#!/bin/sh

DOTFILES_DIR="${HOME}/src/dotfiles"

check_or_create_dir() {
  directory=$1

  if [ -d "${directory}" ]; then
    echo "${directory} already exists, skip..."
  else
    echo "creating ${directory}..."
    mkdir -p "${directory}"
  fi
}

ensure_dirs() {
  check_or_create_dir "${HOME}/bin"
  check_or_create_dir "${HOME}/.config"
}

setup_bin() {
  # link unix bin if it doesn't exist
  if [ -L "${DOTFILES_DIR}/bin/unix" ]; then
    echo "~/bin/unix already setup, skip..."
  else
    ln -s "${DOTFILES_DIR}/bin/unix" "${HOME}/bin/unix"
  fi
}

setup_config() {
  # berrywm
  if [ ! -L "${HOME}/.config/berry" ]; then
    echo "linking config/berry..."
    ln -s "${DOTFILES_DIR}/config/berry" "${HOME}/.config/berry"
  fi

  # sxhkd
  if [ ! -L "${HOME}/.config/sxhkd" ]; then
    echo "linking config/sxhkd..."
    ln -s "${DOTFILES_DIR}/config/sxhkd" "${HOME}/.config/sxhkd"
  fi

  # git
  if [ ! -L "${HOME}/.config/git" ]; then
    echo "linking config/git..."
    ln -s "${DOTFILES_DIR}/config/git" "${HOME}/.config/git"
  fi

  # mpv
  if [ ! -L "${HOME}/.config/mpv" ]; then
    echo "linking config/mpv..."
    ln -s "${DOTFILES_DIR}/config/mpv" "${HOME}/.config/mpv"
  fi
}

setup_xorg() {
  if [ ! -L "${HOME}/.xinitrc" ]; then
    echo "linking xinitrc..."
    ln -s "${DOTFILES_DIR}/config/xorg/xinitrc" "${HOME}/.xinitrc"
  fi

  if [ ! -L "${HOME}/.Xresources" ]; then
    echo "linking Xresources..."
    ln -s "${DOTFILES_DIR}/config/xorg/Xresources" "${HOME}/.Xresources"
  fi
}

setup_emacs() {
  if [ ! -L "${HOME}/.emacs.d" ]; then
    echo "linking emacs.d..."
    ln -s "${DOTFILES_DIR}/config/emacs.d" "${HOME}/.emacs.d" 
  fi
}


install() {
  ensure_dirs
  setup_bin
  setup_config
  setup_xorg
  setup_emacs
}


## MAIN
if [ "$1" = "--install" ]; then
  install
fi
