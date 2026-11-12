#!/usr/bin/env python3
"""Update the Linux Amp Desktop pin from Amp's official Debian repository."""

import base64
import gzip
import re
import urllib.request
from email.parser import Parser
from pathlib import Path


PACKAGES_URL = "https://static.ampcode.com/linux/dists/stable/main/binary-amd64/Packages.gz"
PACKAGE = Path(__file__).resolve().parents[2] / "nixos/pkgs/amp-desktop.nix"


def main():
    with urllib.request.urlopen(PACKAGES_URL, timeout=60) as response:
        index = gzip.decompress(response.read()).decode()
    entries = [Parser().parsestr(entry) for entry in index.strip().split("\n\n")]
    matches = [
        entry
        for entry in entries
        if entry["Package"] == "amp-desktop" and entry["Architecture"] == "amd64"
    ]
    if len(matches) != 1:
        raise ValueError(f"expected one amd64 amp-desktop release, found {len(matches)}")

    release = matches[0]
    version = release["Version"] or ""
    checksum = release["SHA256"] or ""
    if not re.fullmatch(r"[0-9][0-9A-Za-z.+~-]*", version):
        raise ValueError(f"invalid release version: {version!r}")
    if release["Filename"] != f"{version}/amp-desktop-{version}-amd64.deb":
        raise ValueError("release filename does not match the Nix package URL")
    if not re.fullmatch(r"[0-9a-fA-F]{64}", checksum):
        raise ValueError("missing or invalid SHA256 checksum")
    sri = "sha256-" + base64.b64encode(bytes.fromhex(checksum)).decode()

    original = PACKAGE.read_text()
    updated, versions = re.subn(
        r'^  version = "[^"]+";$', f'  version = "{version}";', original, flags=re.MULTILINE
    )
    updated, hashes = re.subn(
        r'^    hash = "[^"]+";$', f'    hash = "{sri}";', updated, flags=re.MULTILINE
    )
    if versions != 1 or hashes != 1:
        raise ValueError("expected exactly one version and source hash in the Nix package")
    if updated == original:
        print(f"Amp Desktop is already up to date ({version})")
    else:
        PACKAGE.write_text(updated)
        print(f"Updated Amp Desktop to {version}")


if __name__ == "__main__":
    main()
