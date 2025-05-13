DEVENV=${HOME}/dev/src/latrokles
DOTFILES=${DEVENV}/dotfiles


OS=$(uname -s)

case "$OS" in
	Darwin)
		echo "Operating System: macOS"
		export PATH=${HOME}/.local/bin:${PATH}
		ln -s ${DOTFILES}/config/emacs.d ~/.emacs.d
		ln -s ${DOTFILES}/config/phoenix.js ~/.phoenix.js
		ln -s ${DOTFILES}/config/zprofile-macos ~/.zprofile
		echo "source ${DOTFILES}/config/zshenv-macos" > ~/.zshenv
		bash ${DOTFILES}/config/defaults-macos

		# setup homebrew
		bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
                eval "$(/opt/homebrew/bin/brew shellenv)"
		brew bundle --file ${DOTFILES}/config/Brewfile
		/opt/homebrew/bin/brew shellenv >> ~/.zshenv

		# setup java
		if [ "$USER" == "latrokles" ]; then
			brew bundle --file ${DOTFILES}/config/Brewfile-java
			echo "export PATH=/opt/homebrew/opt/openjdk/bin:$PATH" >> ~/.zshenv
		fi

		# setup rust
		echo '. "${HOME}/.cargo/env"' >> ~/.zshenv

                echo "done!"
                ;;
	Linux)
		echo "Operating System: Linux"
		;;
	*)
		echo "Operating System: Unsupported"
		;;
esac

