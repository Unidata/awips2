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
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

#
# Tests for the LocalizationFileManager
#
# SOFTWARE HISTORY
#
# Date      Ticket#  Engineer  Description
# --------- -------- --------- --------------------------
# 08/09/17  5731     bsteffen  Initial Creation.

import unittest

from ufpy.localization.LocalizationFileManager import (LocalizationFileManager, 
                                                       LocalizationFileVersionConflictException, 
                                                       LocalizationContext, 
                                                       LocalizationFileIsNotDirectoryException, 
                                                       LocalizationFileDoesNotExistException)

testFile = "purge/defaultPurgeRules.xml"
testContent = "<purgeRuleSet><defaultRule><period>05-05:05:05</period></defaultRule></purgeRuleSet>"
testDir = "purge/"
testNewFile = "purge/testPurgeRules.xml"

class ContextTestCase(unittest.TestCase):
    def test_eq(self):
        c1 = LocalizationContext()
        c2 = LocalizationContext()
        self.assertEqual(c1,c2)
        c3 = LocalizationContext("site", "test")
        c4 = LocalizationContext("site", "test")
        self.assertEqual(c3,c4)
        self.assertNotEqual(c1,c3)

    def test_hash(self):
        c1 = LocalizationContext()
        c2 = LocalizationContext()
        self.assertEqual(hash(c1),hash(c2))
        c3 = LocalizationContext("site", "test")
        c4 = LocalizationContext("site", "test")
        self.assertEqual(hash(c3),hash(c4))

class LFMTestCase(unittest.TestCase):
    def setUp(self):
        self.manager = LocalizationFileManager()
        userFile = self.manager.getSpecific("user", testFile)
        if userFile.exists():
            userFile.delete()
        newFile = self.manager.getSpecific("user", testNewFile)
        if newFile.exists():
            newFile.delete()
    def test_gets(self):
        startingIncremental = self.manager.getIncremental(testFile)
        baseFile = self.manager.getSpecific("base", testFile)
        self.assertEqual(baseFile, startingIncremental[0])
        self.assertTrue(baseFile.exists())
        self.assertFalse(baseFile.isDirectory())
        userFile = self.manager.getSpecific("user", testFile)
        self.assertFalse(userFile.exists())
        with userFile.open("w") as stream:
            stream.write(testContent)
        userFile = self.manager.getSpecific("user", testFile)
        self.assertTrue(userFile.exists())
        with userFile.open('r') as stream:
            self.assertEqual(stream.read(), testContent)
        absFile = self.manager.getAbsolute(testFile)
        self.assertEqual(absFile, userFile)
        endingIncremental = self.manager.getIncremental(testFile)
        self.assertEqual(len(startingIncremental) + 1, len(endingIncremental))
        self.assertEqual(userFile, endingIncremental[-1])
        self.assertEqual(baseFile, endingIncremental[0])

        
        userFile.delete()
        userFile = self.manager.getSpecific("user", testFile)
        self.assertFalse(userFile.exists())

    def test_concurrent_edit(self):
        userFile1 = self.manager.getSpecific("user", testFile)
        userFile2 = self.manager.getSpecific("user", testFile)
        self.assertFalse(userFile1.exists())
        self.assertFalse(userFile2.exists())
        with self.assertRaises(LocalizationFileVersionConflictException):
            with userFile1.open("w") as stream1:
                stream1.write(testContent)
                with userFile2.open("w") as stream2:
                    stream2.write(testContent)

        userFile = self.manager.getSpecific("user", testFile)
        userFile.delete()
        
    def test_dir(self):
        dir = self.manager.getAbsolute(testDir)
        self.assertTrue(dir.isDirectory())
        with self.assertRaises(Exception):
            dir.delete()

    def test_list(self):
        abs1 = self.manager.listAbsolute(testDir)
        inc1 = self.manager.listIncremental(testDir)
        self.assertEqual(len(abs1), len(inc1))
        for i in range(len(abs1)):
            self.assertEquals(abs1[i], inc1[i][-1])
        
        userFile = self.manager.getSpecific("user", testNewFile)
        self.assertNotIn(userFile, abs1)
        
        with userFile.open("w") as stream:
            stream.write(testContent)
        userFile = self.manager.getSpecific("user", testNewFile)

        
        abs2 = self.manager.listAbsolute(testDir)
        inc2 = self.manager.listIncremental(testDir)
        self.assertEqual(len(abs2), len(inc2))
        for i in range(len(abs2)):
            self.assertEquals(abs2[i], inc2[i][-1])
        
        self.assertEquals(len(abs1) + 1, len(abs2))
        self.assertIn(userFile, abs2)
        
        userFile.delete()

    def test_list_file(self):
        with self.assertRaises(LocalizationFileIsNotDirectoryException):
            self.manager.listIncremental(testFile)

    def test_list_nonexistant(self):
        with self.assertRaises(LocalizationFileDoesNotExistException):
            self.manager.listIncremental('dontNameYourDirectoryThis')
        
    def test_root_variants(self):
        list1 = self.manager.listAbsolute(".")
        list2 = self.manager.listAbsolute("")
        list3 = self.manager.listAbsolute("/")
        self.assertEquals(list1,list2)
        self.assertEquals(list2,list3)

    def test_slashiness(self):
        raw = testDir
        if raw[0] == '/':
            raw = raw[1:]
        if raw[-1] == '/':
            raw = raw[:-1]
        list1 = self.manager.listAbsolute(raw)
        list2 = self.manager.listAbsolute(raw + "/")
        list3 = self.manager.listAbsolute("/" + raw)
        self.assertEquals(list1,list2)
        self.assertEquals(list2,list3)
        


if __name__ == '__main__':
    unittest.main()