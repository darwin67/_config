import gzip
import importlib.util
import io
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


spec = importlib.util.spec_from_file_location(
    "updater", Path(__file__).with_name("update-amp-desktop.py")
)
updater = importlib.util.module_from_spec(spec)
spec.loader.exec_module(updater)

PACKAGE = '''  version = "0.1.123";
    hash = "sha256-old";
    url = "https://static.ampcode.com/linux/${finalAttrs.version}/amp-desktop-${finalAttrs.version}-amd64.deb";
'''
RELEASE = '''Package: amp-desktop
Architecture: amd64
Version: 0.1.456
Filename: 0.1.456/amp-desktop-0.1.456-amd64.deb
SHA256: 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f
'''


class UpdateAmpDesktopTest(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.package = Path(directory.name) / "amp-desktop.nix"
        self.package.write_text(PACKAGE)
        package_patch = patch.object(updater, "PACKAGE", self.package)
        package_patch.start()
        self.addCleanup(package_patch.stop)

    def update(self, index):
        with patch.object(
            updater.urllib.request,
            "urlopen",
            return_value=io.BytesIO(gzip.compress(index.encode())),
        ):
            updater.main()

    def test_updates_both_pins_and_is_idempotent(self):
        # An unrelated architecture must not supply the selected release.
        index = RELEASE.replace("amd64", "arm64") + "\n" + RELEASE
        self.update(index)
        expected = PACKAGE.replace("0.1.123", "0.1.456").replace(
            "sha256-old", "sha256-AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8="
        )
        self.assertEqual(self.package.read_text(), expected)
        self.update(index)
        self.assertEqual(self.package.read_text(), expected)

    def test_invalid_metadata_leaves_package_unchanged(self):
        invalid = [
            "",
            RELEASE + "\n" + RELEASE,
            RELEASE.replace("Architecture: amd64", "Architecture: arm64"),
            RELEASE.replace("Version: 0.1.456", 'Version: 0.1.456"'),
            RELEASE.replace("Filename: 0.1.456/", "Filename: other/"),
            RELEASE.replace("SHA256:", "Missing-Checksum:"),
            RELEASE.replace("00010203", "invalid!"),
        ]
        for index in invalid:
            with self.subTest(index=index):
                with self.assertRaises(ValueError):
                    self.update(index)
                self.assertEqual(self.package.read_text(), PACKAGE)

    def test_changed_package_shape_is_not_partially_updated(self):
        original = PACKAGE.replace('    hash = "sha256-old";\n', "")
        self.package.write_text(original)
        with self.assertRaises(ValueError):
            self.update(RELEASE)
        self.assertEqual(self.package.read_text(), original)


if __name__ == "__main__":
    unittest.main()
