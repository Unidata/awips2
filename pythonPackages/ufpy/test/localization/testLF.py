##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     2120 South 72nd Street, Suite 900
#                         Omaha, NE 68124
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

#
# Tests for the lf utility script
#
# SOFTWARE HISTORY
#
# Date      Ticket#  Engineer  Description
# --------- -------- --------- --------------------------
# 01/11/22  8735     mapeters  Initial creation

import os
import subprocess
import unittest

class TxtFileTestCase(unittest.TestCase):
    def setUp(self):
        """Test file needs to not exist at the start of the test. Delete it,
        suppressing any errors (an error would be printed if it doesn't exist).
        """
        self.locDir = "lfTestDir"
        self.locFile = os.path.join(self.locDir, "TestFile.txt")
        subprocess.run(["lf", "rm", "-o", self.locFile], stderr=subprocess.DEVNULL)

    def test_file_operations(self):
        """Run through a typical set of interactions with a .txt file and
        verify everything works correctly.
        """
        subprocess.run(["lf", "write", self.locFile], input=b"Test Data", check=True)

        proc = subprocess.run(["lf", "ls", "-l", self.locDir], stdout=subprocess.PIPE, check=True)
        self.assertIn(self.locFile, proc.stdout.decode())

        proc = subprocess.run(["lf", "read", self.locFile], stdout=subprocess.PIPE, check=True)
        self.assertEqual(proc.stdout.decode(), "Test Data")

        subprocess.run(["lf", "rm", "-o", self.locFile], check=True)

        proc = subprocess.run(["lf", "ls", "-l", self.locDir], stdout=subprocess.PIPE, check=True)
        self.assertNotIn(self.locFile, proc.stdout.decode())

class PngFileTestCase(unittest.TestCase):
    def setUp(self):
        """Test file needs to not exist at the start of the test. Delete it,
        suppressing any errors (an error would be printed if it doesn't exist).
        """
        self.locDir = "lfTestDir"
        self.locFile = os.path.join(self.locDir, "TestImage.png")
        subprocess.run(["lf", "rm", "-o", self.locFile], stderr=subprocess.DEVNULL)

    def test_file_operations(self):
        """Run through a typical set of interactions with a .png file and
        verify everything works correctly.
        """
        cwdPath = os.path.realpath(os.path.join(os.getcwd(), os.path.dirname(__file__)))
        localPngPath = os.path.join(cwdPath, "smallTestImage.png")
        with open(localPngPath, "rb") as f:
            subprocess.run(["lf", "write", self.locFile], stdin=f, check=True)

        proc = subprocess.run(["lf", "ls", self.locDir], stdout=subprocess.PIPE, check=True)
        self.assertIn(self.locFile, proc.stdout.decode().splitlines())

        proc = subprocess.run(["lf", "read", self.locFile], stdout=subprocess.PIPE, check=True)
        with open(localPngPath, "rb") as f:
            localPngBytes = f.read()
        self.assertEqual(proc.stdout, localPngBytes)

        subprocess.run(["lf", "rm", "-o", self.locFile], check=True)

        proc = subprocess.run(["lf", "ls", self.locDir], stdout=subprocess.PIPE, check=True)
        self.assertNotIn(self.locFile, proc.stdout.decode().splitlines())

if __name__ == '__main__':
    unittest.main()
