# Copyright 2019-2019 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later

EAPI=7

CMAKE_MIN_VERSION=3.8.0
inherit cmake-utils git-r3 llvm

DESCRIPTION="C/C++/ObjC language server protocol implementation"
HOMEPAGE="https://github.com/MaskRay/ccls"
EGIT_REPO_URI="https://github.com/MaskRay/${PN}"


if [[ ${PV} == *9999 ]] ; then
	SRC_URI=""
	KEYWORDS=""
else
	SRC_URI="https://github.com/MaskRay/${PN}/archive/v${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="~amd64 ~x86"
fi

LICENSE="Apache-2.0"
SLOT="0"
IUSE=""
REQUIRED_USE=""

DEPEND="
	>=sys-devel/clang-7
	sys-libs/ncurses
"
RDEPEND="${DEPEND}"

llvm_check_deps() {
	has_version -d "sys-devel/clang:${LLVM_SLOT}"
}

src_configure() {
	local mycmakeargs=(
		-DCMAKE_PREFIX_PATH="${EPREFIX}/$(get_llvm_prefix)"
	)

	cmake-utils_src_configure
}
