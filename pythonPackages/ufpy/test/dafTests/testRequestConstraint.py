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

from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint

import unittest

#
# Unit tests for Python implementation of RequestConstraint
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/22/16        2416          tgurney        Initial creation
#
#


class RequestConstraintTestCase(unittest.TestCase):

    def _newRequestConstraint(self, constraintType, constraintValue):
        constraint = RequestConstraint()
        constraint.constraintType = constraintType
        constraint.constraintValue = constraintValue
        return constraint

    def testEvaluateEquals(self):
        new = RequestConstraint.new
        self.assertTrue(new('=', 3).evaluate(3))
        self.assertTrue(new('=', 3).evaluate('3'))
        self.assertTrue(new('=', '3').evaluate(3))
        self.assertTrue(new('=', 12345).evaluate(12345L))
        self.assertTrue(new('=', 'a').evaluate('a'))
        self.assertTrue(new('=', 'a').evaluate(u'a'))
        self.assertTrue(new('=', 1.0001).evaluate(2.0 - 0.999999))
        self.assertTrue(new('=', 1.00001).evaluate(1))
        self.assertFalse(new('=', 'a').evaluate(['a']))
        self.assertFalse(new('=', 'a').evaluate(['b']))
        self.assertFalse(new('=', 3).evaluate(4))
        self.assertFalse(new('=', 4).evaluate(3))
        self.assertFalse(new('=', 'a').evaluate('z'))

    def testEvaluateNotEquals(self):
        new = RequestConstraint.new
        self.assertTrue(new('!=', 'a').evaluate(['a']))
        self.assertTrue(new('!=', 'a').evaluate(['b']))
        self.assertTrue(new('!=', 3).evaluate(4))
        self.assertTrue(new('!=', 4).evaluate(3))
        self.assertTrue(new('!=', 'a').evaluate('z'))
        self.assertFalse(new('!=', 3).evaluate('3'))
        self.assertFalse(new('!=', '3').evaluate(3))
        self.assertFalse(new('!=', 3).evaluate(3))
        self.assertFalse(new('!=', 12345).evaluate(12345L))
        self.assertFalse(new('!=', 'a').evaluate('a'))
        self.assertFalse(new('!=', 'a').evaluate(u'a'))
        self.assertFalse(new('!=', 1.0001).evaluate(2.0 - 0.9999))

    def testEvaluateGreaterThan(self):
        new = RequestConstraint.new
        self.assertTrue(new('>', 1.0001).evaluate(1.0002))
        self.assertTrue(new('>', 'a').evaluate('b'))
        self.assertTrue(new('>', 3).evaluate(4))
        self.assertFalse(new('>', 20).evaluate(3))
        self.assertFalse(new('>', 12345).evaluate(12345L))
        self.assertFalse(new('>', 'a').evaluate('a'))
        self.assertFalse(new('>', 'z').evaluate('a'))
        self.assertFalse(new('>', 4).evaluate(3))

    def testEvaluateGreaterThanEquals(self):
        new = RequestConstraint.new
        self.assertTrue(new('>=', 3).evaluate(3))
        self.assertTrue(new('>=', 12345).evaluate(12345L))
        self.assertTrue(new('>=', 'a').evaluate('a'))
        self.assertTrue(new('>=', 1.0001).evaluate(1.0002))
        self.assertTrue(new('>=', 'a').evaluate('b'))
        self.assertTrue(new('>=', 3).evaluate(20))
        self.assertFalse(new('>=', 1.0001).evaluate(1.0))
        self.assertFalse(new('>=', 'z').evaluate('a'))
        self.assertFalse(new('>=', 40).evaluate(3))

    def testEvaluateLessThan(self):
        new = RequestConstraint.new
        self.assertTrue(new('<', 'z').evaluate('a'))
        self.assertTrue(new('<', 30).evaluate(4))
        self.assertFalse(new('<', 3).evaluate(3))
        self.assertFalse(new('<', 12345).evaluate(12345L))
        self.assertFalse(new('<', 'a').evaluate('a'))
        self.assertFalse(new('<', 1.0001).evaluate(1.0002))
        self.assertFalse(new('<', 'a').evaluate('b'))
        self.assertFalse(new('<', 3).evaluate(40))

    def testEvaluateLessThanEquals(self):
        new = RequestConstraint.new
        self.assertTrue(new('<=', 'z').evaluate('a'))
        self.assertTrue(new('<=', 20).evaluate(3))
        self.assertTrue(new('<=', 3).evaluate(3))
        self.assertTrue(new('<=', 12345).evaluate(12345L))
        self.assertTrue(new('<=', 'a').evaluate('a'))
        self.assertFalse(new('<=', 1.0001).evaluate(1.0002))
        self.assertFalse(new('<=', 'a').evaluate('b'))
        self.assertFalse(new('<=', 4).evaluate(30))

    def testEvaluateIsNull(self):
        new = RequestConstraint.new
        self.assertTrue(new('=', None).evaluate(None))
        self.assertTrue(new('=', None).evaluate('null'))
        self.assertFalse(new('=', None).evaluate(()))
        self.assertFalse(new('=', None).evaluate(0))
        self.assertFalse(new('=', None).evaluate(False))

    def testEvaluateIsNotNull(self):
        new = RequestConstraint.new
        self.assertTrue(new('!=', None).evaluate(()))
        self.assertTrue(new('!=', None).evaluate(0))
        self.assertTrue(new('!=', None).evaluate(False))
        self.assertFalse(new('!=', None).evaluate(None))
        self.assertFalse(new('!=', None).evaluate('null'))

    def testEvaluateIn(self):
        new = RequestConstraint.new
        self.assertTrue(new('in', [3]).evaluate(3))
        self.assertTrue(new('in', ['a', 'b', 3]).evaluate(3))
        self.assertTrue(new('in', 'a').evaluate('a'))
        self.assertTrue(new('in', [3, 4, 5]).evaluate('5'))
        self.assertTrue(new('in', [1.0001, 2, 3]).evaluate(2.0 - 0.9999))
        self.assertFalse(new('in', ['a', 'b', 'c']).evaluate('d'))
        self.assertFalse(new('in', 'a').evaluate('b'))

    def testEvaluateNotIn(self):
        new = RequestConstraint.new
        self.assertTrue(new('not in', ['a', 'b', 'c']).evaluate('d'))
        self.assertTrue(new('not in', [3, 4, 5]).evaluate(6))
        self.assertTrue(new('not in', 'a').evaluate('b'))
        self.assertFalse(new('not in', [3]).evaluate(3))
        self.assertFalse(new('not in', ['a', 'b', 3]).evaluate(3))
        self.assertFalse(new('not in', 'a').evaluate('a'))
        self.assertFalse(new('not in', [1.0001, 2, 3]).evaluate(2.0 - 0.9999))

    def testEvaluateLike(self):
        # cannot make "like" with RequestConstraint.new()
        new = self._newRequestConstraint
        self.assertTrue(new('LIKE', 'a').evaluate('a'))
        self.assertTrue(new('LIKE', 'a%').evaluate('a'))
        self.assertTrue(new('LIKE', 'a%').evaluate('abcd'))
        self.assertTrue(new('LIKE', '%a').evaluate('a'))
        self.assertTrue(new('LIKE', '%a').evaluate('bcda'))
        self.assertTrue(new('LIKE', '%').evaluate(''))
        self.assertTrue(new('LIKE', '%').evaluate('anything'))
        self.assertTrue(new('LIKE', 'a%d').evaluate('ad'))
        self.assertTrue(new('LIKE', 'a%d').evaluate('abcd'))
        self.assertTrue(new('LIKE', 'aa.()!{[]^%$').evaluate('aa.()!{[]^zzz$'))
        self.assertTrue(new('LIKE', 'a__d%').evaluate('abcdefg'))
        self.assertFalse(new('LIKE', 'a%').evaluate('b'))
        self.assertFalse(new('LIKE', 'a%').evaluate('ba'))
        self.assertFalse(new('LIKE', '%a').evaluate('b'))
        self.assertFalse(new('LIKE', '%a').evaluate('ab'))
        self.assertFalse(new('LIKE', 'a%').evaluate('A'))
        self.assertFalse(new('LIKE', 'A%').evaluate('a'))
        self.assertFalse(new('LIKE', 'a%d').evaluate('da'))
        self.assertFalse(new('LIKE', 'a__d%').evaluate('abccdefg'))
        self.assertFalse(new('LIKE', '....').evaluate('aaaa'))
        self.assertFalse(new('LIKE', '.*').evaluate('anything'))

    def testEvaluateILike(self):
        # cannot make "ilike" with RequestConstraint.new()
        new = self._newRequestConstraint
        self.assertTrue(new('ILIKE', 'a').evaluate('a'))
        self.assertTrue(new('ILIKE', 'a%').evaluate('a'))
        self.assertTrue(new('ILIKE', 'a%').evaluate('abcd'))
        self.assertTrue(new('ILIKE', '%a').evaluate('a'))
        self.assertTrue(new('ILIKE', '%a').evaluate('bcda'))
        self.assertTrue(new('ILIKE', '%').evaluate(''))
        self.assertTrue(new('ILIKE', '%').evaluate('anything'))
        self.assertTrue(new('ILIKE', 'a%d').evaluate('ad'))
        self.assertTrue(new('ILIKE', 'a%d').evaluate('abcd'))
        self.assertTrue(new('ILIKE', 'a').evaluate('A'))
        self.assertTrue(new('ILIKE', 'a%').evaluate('A'))
        self.assertTrue(new('ILIKE', 'a%').evaluate('ABCD'))
        self.assertTrue(new('ILIKE', '%a').evaluate('A'))
        self.assertTrue(new('ILIKE', '%a').evaluate('BCDA'))
        self.assertTrue(new('ILIKE', '%').evaluate(''))
        self.assertTrue(new('ILIKE', '%').evaluate('anything'))
        self.assertTrue(new('ILIKE', 'a%d').evaluate('AD'))
        self.assertTrue(new('ILIKE', 'a%d').evaluate('ABCD'))
        self.assertTrue(new('ILIKE', 'A').evaluate('a'))
        self.assertTrue(new('ILIKE', 'A%').evaluate('a'))
        self.assertTrue(new('ILIKE', 'A%').evaluate('abcd'))
        self.assertTrue(new('ILIKE', '%A').evaluate('a'))
        self.assertTrue(new('ILIKE', '%A').evaluate('bcda'))
        self.assertTrue(new('ILIKE', '%').evaluate(''))
        self.assertTrue(new('ILIKE', '%').evaluate('anything'))
        self.assertTrue(new('ILIKE', 'A%D').evaluate('ad'))
        self.assertTrue(new('ILIKE', 'A%D').evaluate('abcd'))
        self.assertTrue(new('ILIKE', 'aa.()!{[]^%$').evaluate('AA.()!{[]^zzz$'))
        self.assertTrue(new('ILIKE', 'a__d%').evaluate('abcdefg'))
        self.assertTrue(new('ILIKE', 'a__d%').evaluate('ABCDEFG'))
        self.assertFalse(new('ILIKE', 'a%').evaluate('b'))
        self.assertFalse(new('ILIKE', 'a%').evaluate('ba'))
        self.assertFalse(new('ILIKE', '%a').evaluate('b'))
        self.assertFalse(new('ILIKE', '%a').evaluate('ab'))
        self.assertFalse(new('ILIKE', 'a%d').evaluate('da'))
        self.assertFalse(new('ILIKE', 'a__d%').evaluate('abccdefg'))
        self.assertFalse(new('ILIKE', '....').evaluate('aaaa'))
        self.assertFalse(new('ILIKE', '.*').evaluate('anything'))

    def testEvaluateBetween(self):
        # cannot make "between" with RequestConstraint.new()
        new = self._newRequestConstraint
        self.assertTrue(new('BETWEEN', '1--1').evaluate(1))
        self.assertTrue(new('BETWEEN', '1--10').evaluate(1))
        self.assertTrue(new('BETWEEN', '1--10').evaluate(5))
        self.assertTrue(new('BETWEEN', '1--10').evaluate(10))
        self.assertTrue(new('BETWEEN', '1.0--1.1').evaluate(1.0))
        self.assertTrue(new('BETWEEN', '1.0--1.1').evaluate(1.05))
        self.assertTrue(new('BETWEEN', '1.0--1.1').evaluate(1.1))
        self.assertTrue(new('BETWEEN', 'a--x').evaluate('a'))
        self.assertTrue(new('BETWEEN', 'a--x').evaluate('j'))
        self.assertTrue(new('BETWEEN', 'a--x').evaluate('x'))
        self.assertFalse(new('BETWEEN', '1--1').evaluate(2))
        self.assertFalse(new('BETWEEN', '1--2').evaluate(10))
        self.assertFalse(new('BETWEEN', '1--10').evaluate(0))
        self.assertFalse(new('BETWEEN', '1--10').evaluate(11))
        self.assertFalse(new('BETWEEN', '1.0--1.1').evaluate(0.99))
        self.assertFalse(new('BETWEEN', '1.0--1.1').evaluate(1.11))
        self.assertFalse(new('BETWEEN', 'a--x').evaluate(' '))
        self.assertFalse(new('BETWEEN', 'a--x').evaluate('z'))

