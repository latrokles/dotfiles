DEVENV=${HOME}/dev/src/latrokles
DOTFILES=${DEVENV}/dotfiles


OS=$(uname -s)

case "$OS" in
	Darwin)
		echo "Operating System: macOS"
		bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
		ln -s ${DOTFILES}/config/emacs.d ~/.emacs.d
		ln -s ${DOTFILES}/config/zshenv-macos ~/.zshenv
		ln -s ${DOTFILES}/config/zprofile-macos ~/.zprofile
		ln -s ${DOTFILES}/config/phoenix.js ~/.phoenix.js
		bash ${DOTFILES}/config/defaults-macos
		
                eval "$(/opt/homebrew/bin/brew shellenv)"
		brew bundle --file ${DOTFILES}/config/Brewfile
		if [ "$USER" == "latrokles" ]; then
			brew bundle --file ${DOTFILES}/config/Brewfile-java
			echo "export PATH=/opt/homebrew/opt/openjdk/bin:$PATH" >> ~/.zshrc
		fi
                echo "done!"
                ;;
	Linux)
		echo "Operating System: Linux"
		;;
	*)
		echo "Operating System: Unsupported"
		;;
esac

