#!/bin/bash -p

#------------------------------------------------------------------------------#

set -u

#------------------------------------------------------------------------------#

if [ -z "${BASH_VERSION:-}" ]; then
    echo "Error: Bash is required to run $0." >&2
    exit 1
fi

#------------------------------------------------------------------------------#

set +o posix

#------------------------------------------------------------------------------#

if [ ! -z "${CS1302_ENV_SCRIPT-}" ]; then
    echo "Error: The cs302env environment is already activated!" >&2
    exit 1
fi

#------------------------------------------------------------------------------#

# Script meta
declare -r CS1302_ENV_SCRIPT="${0}"
declare -r CS1302_ENV_SCRIPT_NAME="CSCI 1302 macOS Environment Script"
declare -r CS1302_ENV_SCRIPT_VERSION="Spring 2024"
declare -r CS1302_ENV_SCRIPT_USER_DIR="${HOME}/.cs1302-env-macos-2024sp"
declare -r CS1302_ENV_PROCESSOR="$(uname -m)"
declare -r CS1302_ENV_SYSTEM="$(uname -s)"

# Shorter version of script name used in some labels
declare -r CS1302_ENV_SCRIPT_PROG="$(basename ${CS1302_ENV_SCRIPT})"

# Detect the operating system
case "${OSTYPE}" in
    darwin*)
	declare -r CS1302_ENV_MACOS="1";
	declare -r CS1302_ENV_OS="MACOS"
	declare -r CS1302_ENV_OS_NAME="$(sw_vers --productName)"
	declare -r CS1302_ENV_OS_VERSION="$(sw_vers --productVersion)"
	declare -r CS1302_ENV_JDK_OS="macos"
	;;
    *)
	echo "Error: This script only supports macOS, but ${OSTYPE} detected." >&2
	exit 1
	;;
esac

# Directory for generated shell scripts
declare -r CS1302_ENV_SCRIPT_USER_DIR_BIN="${CS1302_ENV_SCRIPT_USER_DIR}/bin"
declare -r CS1302_ENV_SCRIPT_USER_DIR_LIB="${CS1302_ENV_SCRIPT_USER_DIR}/lib"

# Download directories
declare -r CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS="${CS1302_ENV_SCRIPT_USER_DIR}/downloads"
declare -r CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_JDK="${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS}/jdk"
declare -r CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_MAVEN="${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS}/apache-maven"

# Curl
declare -r CS1302_ENV_CURL_VERSION_MIN="7.41.0"

# JDK version to use
declare -r CS1302_ENV_JDK_VERSION="17.0.10"
declare -r CS1302_ENV_JDK_VERSION_MAJOR="17"
declare -r CS1302_ENV_JDK_VERSION_MINOR="0"
declare -r CS1302_ENV_JDK_VERSION_PATCH="10"

# Adjust the JDK architecture based on the type of processor
case "${CS1302_ENV_PROCESSOR}" in
    x86_64)
	declare -r CS1302_ENV_JDK_ARCH="x86"
	;;
    arm64)
	declare -r CS1302_ENV_JDK_ARCH="aarch64"
	;;
    *)
	echo "Error: Unable to determine which JDK architecture is needed" \
	     "based on your machine's processor: ${CS1302_ENV_PROCESSOR}." \
	     "Please see an instructor for assistance." >&2
	exit 1
	;;
esac

# Maven version to use
declare -r CS1302_ENV_MVN_VERSION="3.9.6"
declare -r CS1302_ENV_MVN_VERSION_MAJOR="3"
declare -r CS1302_ENV_MVN_VERSION_MINOR="9"
declare -r CS1302_ENV_MVN_VERSION_PATCH="6"

# MANPTH
export PATH="${PATH:-/usr/bin:/bin:/usr/sbin:/sbin}"
export MANPATH="${MANPATH:-"$(manpath)"}"

#------------------------------------------------------------------------------#

which() {
    #
    # SYNOPSIS:
    #     which program ...
    #
    # Locate and display program executables in the user's PATH.
    #
    # Return with status 0 if paths to all the given program executable can be
    # resolved; otherwise, return with exit status 1.
    #
    type -P $@
} # which

exists_on_path() {
    #
    # SYNOPSIS:
    #     exists_on_path program ...
    #
    # Return with status 0 if paths to the given program executables can be
    # resolved; otherwise, return with exit status 1.
    #
    which $@ &>/dev/null
} # exists_on_path

silent_cd() {
    #
    # SYNOPSIS:
    #     silent_cd path
    #
    # Silently set the current working directory.
    #
    cd "${1}" &>/dev/null
} # silent_cd

realpath() {
    #
    # SYNOPSIS:
    #     realpath [-s] [path ...]
    #
    # Display the real path to one or more files or directories, with all
    # symbolic links, extra ‘/’ characters, and references to /./ and /../ in
    # each path resolved. This portable version of 'realpath' does not rely on
    # the 'realpath' executable provided by GNU coreutils.
    #
    # If -s is specified, errors and warnings will not be printed when a real
    # path cannot be resolved.
    #
    # Return with status 0 if a path can be resolved; otherwise, return with
    # exit status 1.
    #
    local SILENT="0"
    local STATUS="0"
    # check for -s
    if [[ $# -eq 2 ]] && [[ "${1}" == "-s" ]]; then
	SILENT="1"
	shift
    fi
    # find and display the real paths
    for FILE in "${@:-.}"; do
	if [[ -d "${FILE}" ]]; then
	    echo "$(silent_cd "${FILE}" && pwd -P)"
	elif [[ -f "${FILE}" ]]; then
	    local DIRNAME="$(dirname "${FILE}")"
	    local BASENAME="$(basename "${FILE}")"
	    echo "$(silent_cd "${DIRNAME}" && pwd -P)/${BASENAME}"
	else
	    if [[ "${SILENT}" != "1" ]]; then
		echo "realpath: ${FILE}: No such file or directory" 1>&2
	    fi
	    STATUS="1"
	fi
    done
    return "${STATUS}"
} # realpath

is_version_gte() {
    #
    # SYNOPSIS:
    #     is_version_gte version1 version2
    #
    # Return with status 0 if version1 is considered greater than or equal to
    # version2; otherwise, return with exit status 1.
    #
    local VERSION1="${1}"
    local VERSION2="${2}"
    [[ "$(printf '%s\n%s\n' "${VERSION1}" "${VERSION2}" | sort -V | head -n 1)" == "${VERSION2}" ]]
} # is_version_eq

stdout_isatty() {
    #
    # SYNOPSIS:
    #     stdout_isatty
    #
    # Return with status 0 if standard output is a TTY; otherwise, return with
    # exit status 1.
    #
    test -t 1
} # stdout_isatty

# Display text as bold text, if supported, else simply display the text.
echo_bold() {
    #
    # SYNOPSIS:
    #     echo_bold text
    #
    # TODO...
    #
    if stdout_isatty; then
	echo -e "\\033[1m""$*""\\033[0m"
    else
	echo "$*"
    fi
} # echo_bold

# Display text as bold red text, if supported, else simply display the text.
echo_bold_red() {
    if stdout_isatty; then
	echo -e "\\033[1;31m""$*""\\033[0m"
    else
	echo "$*"
    fi
} # echo_bold_red

# Display a header containing an arrow and supplied text.
echo_task() {
    if stdout_isatty; then
	# bold blue arrow and bold text, like Homebrew
	echo -e "\\033[1;34m==>\\033[0m \\033[1m$*\\033[0m"
    else
	# only show arrow and text
	echo "==> $*"
    fi
} # echo_task

echo_item() {
    if stdout_isatty; then
	# bold blue arrow and regular text
	echo -e "\\033[34m-->\\033[0m $*"
    else
	# only show arrow and text
	echo "--> $*"
    fi
} # echo_item

echo_error() {
    if stdout_isatty; then
	# bold red '!!>' arrow,  bold red 'ERROR:' label, then regular text
	echo -e "\\033[1;31m!!>\\033[0m \\033[1;31mERROR:\\033[0m $*"
    else
	echo "!!> ERROR: $*"
    fi
} # echo_error

echo_error_exit() {
    echo_error $@
    exit 1
} # echo_error_exit

echo_welcome() {
    printf '%s\n' \
	 '          _ _____  ___ ____  ' \
	 '  ___ ___/ |___ / / _ \___ \ ' \
	 ' / __/ __| | |_ \| | | |__) |' \
	 '| (__\__ \ |___) | |_| / __/ ' \
	 ' \___|___/_|____/ \___/_____|'
    echo ""
    echo_bold ${CS1302_ENV_SCRIPT_NAME}
    echo_item Script Version: ${CS1302_ENV_SCRIPT_VERSION}
    echo_item Operating System: ${CS1302_ENV_OS_NAME} ${CS1302_ENV_OS_VERSION}
    echo_item Processor: ${CS1302_ENV_PROCESSOR}
    echo_item System: ${CS1302_ENV_SYSTEM}
} # echo_welcome

path_contains() {
    #
    # SYNOPSIS:
    #     path_contains dir
    #
    local DIR_VERBATIM="${1}"
    local DIR="$(realpath -s "${DIR_VERBATIM}")"
    case ":${PATH}:" in
	*":${DIR_VERBATIM}:"*|*":${DIR}:"*)
	    return 0;
	    ;;
	*)
	    return 1;
	    ;;
    esac
} # path_contains

prepend_path_brew_formula() {
    #
    # SYNOPSIS:
    #     path_prepend_brew_formula formula
    #
    # Locate the 'bin' and 'man' directories for an installed Homebrew formula,
    # then and add them to the user's PATH and MANPATH.
    #
    local FORMULA="${1}"
    local FORMULA_PREFIX=$(brew --prefix --installed "${FORMULA}")
    local FORMULA_BIN="$(find -H ${FORMULA_PREFIX} -type d -name "bin" | head -n1)"
    local FORMULA_MAN="$(find -H ${FORMULA_PREFIX} -type d -name "man" | head -n1)"
    if [[ -d "${FORMULA_BIN}" ]]; then
	export PATH="${FORMULA_BIN}:${PATH}"
    fi
    if [[ -d "${FORMULA_MAN}" ]]; then
	export MANPATH="${FORMULA_MAN}:${MANPATH}"
    fi
} # path_prepend_brew_formula

yes_or_no() {
    #
    # Inspired by Emacs 'yes-or-no-p' function, this 'yes_or_no' function
    # displays text to the user, prefixed with a '??>' arrow and suffixed with
    # '(yes or no)'. It returns an exit status of 0 if the user enters 'yes' and
    # 1 if the user enters 'no'. Upper and lower case are equivalent.
    #
    # The user must type one of the expected responses; otherwise, the function
    # displays ‘Please answer yes or no.’, waits about two seconds, then repeats
    # the request.
    #
    # Here is an example:
    #
    #     yes_or_no Run the Homebrew installer?
    #
    # The expected output is:
    #
    #     ??> Run the Homebrew installer? (yes or no)
    #
    local RESPONSE=""
    while [[ ! -n "${RESPONSE}" ]]; do
	if stdout_isatty; then
	    # bold purple ??> arrow, bold  and regular bold text
	    printf "\\033[1;35m??>\\033[0m \\033[1m$*\\033[0m \\033[1;35m(yes or no)\\033[0m "
	else
	    # only show arrow and text
	    printf "??> $* (yes or no) "
	fi
	read -r RESPONSE
	case "${RESPONSE}" in
	    [yY][eE][sS])
		RESPONSE="yes" # normalize
		;;
	    [nN][oO])
		RESPONSE="no" # normalize
		;;
	    *)
		echo "Please answer yes or no."
		RESPONSE="" # reset
		sleep 2
		;;
	esac
    done
    [[ "${RESPONSE}" == "yes" ]] # return with 0 if "yes", else 1
} # yes_or_no

curl() {
    local CURL_EXE="$(which curl)"
    "${CURL_EXE}" "$@"
} # curl

curl_download() {
    local URL="${1}"
    local FILE="${2}"
    tput sc >&2
    curl \
	-L \
	-H 'Cache-Control: no-cache, no-store' \
	-H 'Pragma: no-cache' \
	--progress-bar \
	"${URL}" >"${FILE}"
    tput rc >&2
    tput ed >&2
} # curl_download

curl_version() {
    local CURL_VERSION_OUTPUT="$(curl --version 2>/dev/null)"
    echo "${CURL_VERSION_OUTPUT%% (*}" 2>/dev/null
} # curl_version

jdk_archive_url() {
    #
    # Display the download URL for the JDK archive.
    #
    printf 'https://%s/%d/archive/jdk-%d.%d.%d_%s-%s_bin.tar.gz\n' \
	   "download.oracle.com/java" \
	   "${CS1302_ENV_JDK_VERSION_MAJOR}" \
	   "${CS1302_ENV_JDK_VERSION_MAJOR}" \
	   "${CS1302_ENV_JDK_VERSION_MINOR}" \
	   "${CS1302_ENV_JDK_VERSION_PATCH}" \
	   "${CS1302_ENV_JDK_OS}" \
	   "${CS1302_ENV_JDK_ARCH}"
} # jdk_archive_url

maven_archive_url() {
    #
    # Display the download URL for the Apache Maven archive.
    #
    printf 'https://%s/maven/maven-%d/%s/binaries/apache-maven-%s-bin.tar.gz\n' \
	   "dlcdn.apache.org" \
	   "${CS1302_ENV_MVN_VERSION_MAJOR}" \
	   "${CS1302_ENV_MVN_VERSION}" \
	   "${CS1302_ENV_MVN_VERSION}"
} # maven_archive_url

task_check_curl() {
    if curl_version &>/dev/null; then
	local CURL_VERSION="$(curl_version)"
	local MIN_VERSION="${CS1302_ENV_CURL_VERSION_MIN}"
	local ACTUAL_VERSION="${CURL_VERSION##* }"
	if is_version_gte "${MIN_VERSION}" "${ACTUAL_VERSION}"; then
	    echo_error_exit "curl ${ACTUAL_VERSION} found, but ${MIN_VERSION} or" \
			    "newer is required. Please update your system curl." \
			    "Usually, the easiest way to do that is by making sure" \
			    "your macOS installation is completely up to date, which" \
			    "can done by running the following command:\n\n" \
			    "    softwareupdate --product-types macOS -i -a'\n"
	else
	    echo_item Found: ${CURL_VERSION}
	fi
    else
        echo_error_exit Unable to locate curl. Please see an instructor for \
			assistance.
    fi
} # task_check_curl

task_check_homebrew() {
    echo_task "Checking for Homebrew..."
    if ! exists_on_path brew; then
	if yes_or_no "Unable to locate a Homebrew installation. Run the installer?"; then
	    task_item "Running Homebrew installer..."
	    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	else
	    echo_error_exit "Unable to continue. Homebrew is a required dependency."
	fi
    fi
    eval "$(/opt/homebrew/bin/brew shellenv)"
    echo_item Found: "$(brew --version)"
} # task_check_homebrew

task_check_homebrew_tap() {
    local TAP="${1}"
    echo_task "Checking for Homebrew tap ${TAP}..."
    # tap cask, if needed
    if ! brew tap | grep "${TAP}" &>/dev/null; then
	echo_item "Tapping Homebrew tap ${TAP}..."
	if ! brew tap "${TAP}" &>/dev/null; then
	    echo_error_exit "Unable to tap required Homebrew tap ${TAP}." \
			    "Please see an instructor for assistance."
	fi
	brew update
    fi
    echo_item "${TAP} is tapped."
} # task_check_homebrew_tap

task_check_homebrew_formula() {
    local FORMULA="${1}"; shift
    local OPTIONS="${@:-}"
    echo_task "Checking for Homebrew formula ${FORMULA}..."
    # install formula, if needed
    if ! brew list --formula | grep "${FORMULA}" &>/dev/null; then
	echo_item "Installing Homebrew formula ${FORMULA}..."
	if ! brew install ${FORMULA} ${OPTIONS}; then
	   echo_error_exit "Unable to install required Homebrew formula ${FORMULA}." \
			   "Please see an instructor for assistance."
	fi
    fi
    # show the formula
    echo_item Found: "$(brew --prefix "${FORMULA}")"
    prepend_path_brew_formula "${FORMULA}"
} # task_check_homebrew_formula

task_check_homebrew_cask() {
    local CASK="${1}"; shift
    local OPTIONS="${@:-}"
    echo_task "Checking for Homebrew cask ${CASK}..."
    # install formula, if needed
    if ! brew list --cask | grep "${CASK}" &>/dev/null; then
	echo_item "Installing Homebrew formula ${CASK}..."
	if ! brew install --cask ${CASK} ${OPTIONS}; then
	   echo_error_exit "Unable to install required Homebrew formula ${CASK}." \
			   "Please see an instructor for assistance."
	fi
    fi
    # show the cask
    echo_item "Found: ${CASK}"
} # task_check_homebrew_cask

task_check_deps() {
    echo_task Checking for required system dependencies...
    task_check_curl
    task_check_homebrew
    task_check_homebrew_formula help2man
    task_check_homebrew_formula git
    task_check_homebrew_formula wget
    task_check_homebrew_formula jq
    task_check_homebrew_formula jc
    task_check_homebrew_tap homebrew/cask-fonts
    task_check_homebrew_cask font-recursive
    task_check_homebrew_tap d12frosted/emacs-plus
    task_check_homebrew_formula emacs-plus@29 --with-mailutils --with-imagemagick --with-native-comp --with-poll --with-xwidgets
} # task_check_deps

task_check_user_dir() {
    echo_task Checking for this script\'s user directory...
    local USER_DIR="${CS1302_ENV_SCRIPT_USER_DIR}"
    if [[ ! -d "${USER_DIR}" ]]; then
	if ! mkdir -p "${USER_DIR}"; then
	    echo_error_exit Unable to create directory "${USER_DIR}" \
			    to use as the script\'s user directory. \
			    Please see an instructor for assistance.
	fi
    fi
    echo_item Using "${USER_DIR}" as the script user directory.
    # create other directories, as needed
    mkdir -p "${CS1302_ENV_SCRIPT_USER_DIR_BIN}" \
	&& mkdir -p "${CS1302_ENV_SCRIPT_USER_DIR_LIB}" \
	&& mkdir -p "${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS}" \
	&& mkdir -p "${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_JDK}" \
	&& mkdir -p "${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_MAVEN}" \
	&& echo_item Using "${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS}" for script downloads. \
    	    || echo_error_exit Unable to create one or more download directores. \
			       Please see an instructor for assistance.
    # add scripts directory to path

} # task_check_user_dir

task_check_jdk() {
    local JDK_VERSION="${CS1302_ENV_JDK_VERSION}"
    local JDK_NAME="jdk-${JDK_VERSION}"
    local JDK_HOME="${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_JDK}/${JDK_NAME}"
    local JDK_ARCHIVE="${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_JDK}/${JDK_NAME}.tar.gz"
    local JDK_URL="$(jdk_archive_url)"
    echo_task "Checking for Oracle JDK..."
    if [[ ! -d "${JDK_HOME}/bin" ]]; then
	echo_item "It looks like this script has not downloaded Oracle JDK yet."
	if yes_or_no "Download and install ${JDK_NAME}?"; then
	    echo_item Downloading: "${JDK_URL}"
	    curl --progress-bar -o "${JDK_ARCHIVE}" "${JDK_URL}"
	    mkdir -p "${JDK_HOME}"
	    if [[ "${CS1302_ENV_OS}" == "MACOS" ]]; then
		local STRIP_COUNT="4"
	    else
		local STRIP_COUNT="2"
	    fi
	    echo_item Extracting: "${JDK_ARCHIVE}"
	    tar -z -x --strip-components "${STRIP_COUNT}" --cd "${JDK_HOME}" -f "${JDK_ARCHIVE}"
	else
	    echo_error_exit "Unable to continue." \
			    "A version of Oracle JDK installed by this script" \
			    "is required."
	fi
    fi
    if [[ ! -d "${JDK_HOME}/bin" ]]; then
	echo_error_exit Unable to setup Oracle JDK. \
			Please see an instructor for assistance.
    fi
    echo_item Found: "${JDK_HOME}/bin"
    # setup paths
    local JDK_PATH="${JDK_HOME}/bin"
    local JDK_MANPATH="${JDK_HOME}/share/man"
    export PATH="${JDK_PATH}:${PATH}"
    # generate manual pages
    if [[ ! -d "${JDK_MANPATH}" ]]; then
	echo_item Generating JDK manual pages...
	mkdir -p "${JDK_MANPATH}/man1"
	for EXE in $(find "${JDK_PATH}" -type f -perm +111); do
	    local EXE_NAME="$(basename ${EXE})"
	    if [[ "${EXE_NAME}" != "jconsole" ]]; then
		if "${EXE}" --help &>/dev/null; then
		    help2man  --no-discard-stderr \
			      --no-info \
			      -n "${EXE_NAME}" \
			      -s "Oracle Corporation" \
			      "${EXE}" > "${JDK_MANPATH}/man1/${EXE_NAME}.1"
		fi
	    fi
	done
    fi
    export MANPATH="${JDK_MANPATH}:${MANPATH}"
    # show versions
    echo_item Found: "$(java --version | head -n 1)"
    echo_item Found: "$(javac --version | head -n 1)"
} # task_check_jdk

task_check_maven() {
    local MAVEN_VERSION="${CS1302_ENV_MVN_VERSION}"
    local MAVEN_NAME="apache-maven-${MAVEN_VERSION}"
    local MAVEN_HOME="${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_MAVEN}/${MAVEN_NAME}"
    local MAVEN_ARCHIVE="${CS1302_ENV_SCRIPT_USER_DIR_DOWNLOADS_MAVEN}/${MAVEN_NAME}.tar.gz"
    local MAVEN_URL="$(maven_archive_url)"
    echo_task Checking for Apache Maven...
    if [[ ! -d "${MAVEN_HOME}/bin" ]]; then
	echo_item "It looks like this script has not downloaded Apache Maven yet."
	if yes_or_no "Download and install ${MAVEN_NAME}?"; then
	    echo_item Downloading: "${MAVEN_URL}"
	    curl --progress-bar -o "${MAVEN_ARCHIVE}" "${MAVEN_URL}"
	    mkdir -p "${MAVEN_HOME}"
	    echo_item Extracting: "${MAVEN_ARCHIVE}"
	    tar -z -x --strip-components 1 --cd "${MAVEN_HOME}" -f "${MAVEN_ARCHIVE}"
	else
	    echo_error_exit "Unable to continue." \
			    "A version of Apache Maven installed by this script" \
			    "is required."
	fi
    fi
    if [[ ! -d "${MAVEN_HOME}/bin" ]]; then
	echo_error_exit Unable to setup Maven. \
			Please see an instructor for assistance.
    fi
    echo_item Exporting to PATH: "${MAVEN_HOME}/bin"
    export PATH="${MAVEN_HOME}/bin:${PATH}"
    echo_item Found: "$(mvn --version | head -n 1 | cut -d' ' -f1-3)"
} # task_check_maven

fetch_elisp() {
    local EMACS_USER_DIR="${CS1302_ENV_SCRIPT_USER_DIR}/.emacs.d"
    local FILE="${1}"
    local URL="https://raw.githubusercontent.com/cs1302uga/cs1302-env-macos/main/share/emacs/${FILE}"
    mkdir -p "${EMACS_USER_DIR}"
    echo_item "Downloading latest ${FILE}..."
    if ! curl_download "${URL}?$(date +%s)" "${EMACS_USER_DIR}/${FILE}"; then
	echo_error_exit "Unable to continue." \
			"There was a problem fetching the required Elisp file" \
			"${FILE} from ${URL}." \
			"Please see an instructor for assisstance"
    fi
} # fetch_elisp

fetch_style_guide_xml() {
    local CS1302_CHECKS_XML="${CS1302_ENV_SCRIPT_USER_DIR_LIB}/cs1302_checks.xml"
    local CS1302_CHECKS_URL='https://raw.githubusercontent.com/cs1302uga/cs1302-styleguide/master/cs1302_checks.xml'
    echo_item "Downloading style guide XML..."
    # if ! curl \
    # 	 -L \
    # 	 -H 'Cache-Control: no-cache, no-store' \
    # 	 -H 'Pragma: no-cache' \
    # 	 --progress-bar \
    if ! curl_download "${CS1302_CHECKS_URL}?$(date +%s)" "${CS1302_CHECKS_XML}"; then
	echo_error_exit "Unable to continue." \
			"There was a problem fetching the style guide" \
			"XML file from ${CS1302_CHECKS_URL}." \
			"Please see an instructor for assisstance"
    fi
} # fetch_style_guide_xml

fetch_checkstyle_jar() {
    local CHECKSTYLE_JAR="${CS1302_ENV_SCRIPT_USER_DIR_LIB}/checkstyle.jar"
    local CHECKSTYLE_URL='https://github.com/checkstyle/checkstyle/releases/download/checkstyle-10.15.0/checkstyle-10.15.0-all.jar'
    echo_item "Downloading checkstyle JAR..."
    if ! curl \
	 -L \
	 -H 'Cache-Control: no-cache, no-store' \
	 -H 'Pragma: no-cache' \
	 --progress-bar \
	 "${CHECKSTYLE_URL}?$(date +%s)" >"${CHECKSTYLE_JAR}"; then
	echo_error_exit "Unable to continue." \
			"There was a problem fetching chekstyle" \
			"JAR from ${CHECKSTYLE_URL}." \
			"Please see an instructor for assisstance"
    fi
} # fetch_checkstyle_jar

task_setup_user_scripts() {
    echo_task "Setting up user scripts and fetching additional files..."
    # emacs elisp files
    local EMACS_USER_DIR="${CS1302_ENV_SCRIPT_USER_DIR}/.emacs.d"
    fetch_elisp init.el
    fetch_elisp site-start.el
    # emacs executable script
    local EMACS_EXE="${CS1302_ENV_SCRIPT_USER_DIR_BIN}/emacs"
    mkdir -p "${EMACS_USER_DIR}"
    cat <<-EOF >"${EMACS_EXE}"
#!/bin/bash

"$(brew --prefix emacs-plus)/bin/emacs" --init-directory "${EMACS_USER_DIR}" -nw \$@
EOF
    chmod +x "${EMACS_EXE}"
    echo_item Generated user script for emacs.
    echo_item "Updating emacs packages (this may take a while)..."
    2>&1 "${EMACS_EXE}" --batch --eval '(package-initialize)' --load "${EMACS_USER_DIR}/init.el" --eval '(package-upgrade-all)' \
	| sed 's|^|    [emacs] |g'
    # checkstyle
    fetch_checkstyle_jar
    local CHECKSTYLE_JAR="${CS1302_ENV_SCRIPT_USER_DIR_LIB}/checkstyle.jar"
    local CHECKSTYLE_EXE="${CS1302_ENV_SCRIPT_USER_DIR_BIN}/checkstyle"
    cat <<-EOF >"${CHECKSTYLE_EXE}"
#!/bin/bash

java -jar "${CHECKSTYLE_JAR}" \$@
EOF
    chmod +x "${CHECKSTYLE_EXE}"
    echo_item Generated user scripts for checkstyle.
    # check1302
    fetch_style_guide_xml
    local CHECK1302_XML="${CS1302_ENV_SCRIPT_USER_DIR_LIB}/check1302_checks.xml"
    local CHECK1302_EXE="${CS1302_ENV_SCRIPT_USER_DIR_BIN}/check1302"
    cat <<-EOF >"${CHECK1302_EXE}"
#!/bin/bash

"${CHECKSTYLE_EXE}" -c "${CHECK1302_XML}" \$@
EOF
    chmod +x "${CHECK1302_EXE}"
    echo_item Generated user script for check1302.
} # task_setup_user_scripts

task_activate() {
    echo_task Activating the environment...
    echo_item Once activated, you can run 'exit' to leave.
    local LABEL="${CS1302_ENV_SCRIPT_PROG/.sh/}"
    export PATH="${CS1302_ENV_SCRIPT_USER_DIR_BIN}:${PATH}"
    export CS1302_ENV_SCRIPT
    export -f echo_error_exit
    case "${SHELL}" in
	*zsh)
	    export PROMPT="[${LABEL}] ${PROMPT}"
	    "${SHELL}" -d -f
	    ;;
	*bash)
	    export PS1="[${LABEL}] ${PS1:-\\[\\h:\\W \\u \\$ }"
	    "${SHELL}" --noprofile
	    ;;
	*)
	    echo_error_exit Unable to spawn environment using "SHELL=${SHELL}".
	    ;;
    esac
} # task_activate

main() {
    echo_welcome
    task_check_deps
    task_check_user_dir
    task_check_jdk
    task_check_maven
    task_setup_user_scripts
    task_activate
    echo_task Deactivating...
    echo_item To reuse this environment, run this "${CS1302_ENV_SCRIPT}" \
	      script again.
} # main

main $@
