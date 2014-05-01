package org.apache.qpid.server.exchange.headers;

import org.apache.qpid.framing.*;

import java.util.*;

/*
*
* Licensed to the Apache Software Foundation (ASF) under one
* or more contributor license agreements.  See the NOTICE file
* distributed with this work for additional information
* regarding copyright ownership.  The ASF licenses this file
* to you under the Apache License, Version 2.0 (the
* "License"); you may not use this file except in compliance
* with the License.  You may obtain a copy of the License at
*
*   http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*
*/
public class HeadersParser
{

    private final HeaderKeyDictionary _dictionary = new HeaderKeyDictionary();
    private static final AMQShortString MATCHING_TYPE_KEY = new AMQShortString("x-match");
    private static final String ANY_MATCHING = "any";
    private static final AMQShortString RESERVED_KEY_PREFIX = new AMQShortString("x-");


    HeadersMatcherDFAState createStateMachine(FieldTable bindingArguments, HeaderMatcherResult result)
    {
        String matchingType = bindingArguments.getString(MATCHING_TYPE_KEY);
        boolean matchAny = matchingType.equalsIgnoreCase(ANY_MATCHING);
        if(matchAny)
        {
            return createStateMachineForAnyMatch(bindingArguments, result);
        }
        else
        {
            return createStateMachineForAllMatch(bindingArguments, result);
        }


    }


    private HeadersMatcherDFAState createStateMachineForAnyMatch(final FieldTable bindingArguments,
                                                                 final HeaderMatcherResult result)
    {

        // DFAs for "any" matches have only two states, "not-matched" and "matched"... they start in the former
        // and upon meeting any of the criteria they move to the latter

        //noinspection unchecked
        final HeadersMatcherDFAState successState =
                new HeadersMatcherDFAState(Collections.EMPTY_MAP,Collections.singleton(result),_dictionary);

        Map<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>> nextStateMap =
                new HashMap<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>>();

        Set<AMQShortString> seenKeys = new HashSet<AMQShortString>();

        Iterator<Map.Entry<AMQShortString, AMQTypedValue>> tableIterator = bindingArguments.iterator();

        while(tableIterator.hasNext())
        {
            final Map.Entry<AMQShortString, AMQTypedValue> entry = tableIterator.next();
            final AMQShortString key = entry.getKey();
            final AMQTypedValue value = entry.getValue();


            if(seenKeys.add(key) && !key.startsWith(RESERVED_KEY_PREFIX))
            {
                final AMQType type = value.getType();

                final HeaderKey headerKey = _dictionary.getOrCreate(key);
                final Map<AMQTypedValue, HeadersMatcherDFAState> valueMap;

                if(type == AMQType.VOID ||
                   ((type == AMQType.ASCII_STRING || type == AMQType.WIDE_STRING) && ((CharSequence)value.getValue()).length() == 0))
                {
                    valueMap = Collections.singletonMap(null,successState);

                }
                else
                {
                    valueMap = Collections.singletonMap(value,successState);
                }
                nextStateMap.put(headerKey,valueMap);

            }

        }

        if(seenKeys.size() == 0)
        {
            return successState;
        }
        else
        {
            return new HeadersMatcherDFAState(nextStateMap,Collections.EMPTY_SET,_dictionary);
        }


    }


    private HeadersMatcherDFAState createStateMachineForAllMatch(final FieldTable bindingArguments,
                                                                 final HeaderMatcherResult result)
    {
        // DFAs for "all" matches have a "success" state, a "fail" state, and states for every subset of
        // matches which are possible, starting with the empty subset.  For example if we have a binding
        //  { x-match="all"
        //          a=1
        //          b=1
        //          c=1
        //          d=1 }
        // Then we would have the following states
        // (1) Seen none of a, b, c, or d
        // (2) Seen a=1 ; none of b,c, or d
        // (3) Seen b=1 ; none of a,c, or d
        // (4) Seen c=1 ; none of a,b, or d
        // (5) Seen d=1 ; none of a,b, or c
        // (6) Seen a=1,b=1 ; none of c,d
        // (7) Seen a=1,c=1 ; none of b,d
        // (8) Seen a=1,d=1 ; none of b,c
        // (9) Seen b=1,c=1 ; none of a,d
        //(10) Seen b=1,d=1 ; none of c,d
        //(11) Seen c=1,d=1 ; none of a,b
        //(12) Seen a=1,b=1,c=1 ; not d
        //(13) Seen a=1,b=1,d=1 ; not c
        //(14) Seen a=1,c=1,d=1 ; not b
        //(15) Seen b=1,c=1,d=1 ; not a
        //(16) success
        //(17) fail
        //
        // All states but (16) can transition to (17); additionally:
        // (1) can transition to (2),(3),(4),(5)
        // (2) can transition to (6),(7),(8)
        // (3) can transition to (6),(9),(10)
        // (4) can transition to (7),(9),(11)
        // (5) can transition to (8),(10),(11)
        // (6) can transition to (12),(13)
        // (7) can transition to (12),(14)
        // (8) can transition to (13),(14)
        // (9) can transition to (12),(15)
        //(10) can transition to (13),(15)
        //(11) can transition to (14),(15)
        //(12)-(15) can transition to (16)

        Set<AMQShortString> seenKeys = new HashSet<AMQShortString>();
        List<KeyValuePair> requiredTerms = new ArrayList<KeyValuePair>(bindingArguments.size());

        Iterator<Map.Entry<AMQShortString, AMQTypedValue>> tableIterator = bindingArguments.iterator();



        while(tableIterator.hasNext())
        {
            final Map.Entry<AMQShortString, AMQTypedValue> entry = tableIterator.next();
            final AMQShortString key = entry.getKey();
            final AMQTypedValue value = entry.getValue();


            if(seenKeys.add(key) && !key.startsWith(RESERVED_KEY_PREFIX))
            {
                final AMQType type = value.getType();

                if(type == AMQType.VOID ||
                   ((type == AMQType.ASCII_STRING || type == AMQType.WIDE_STRING) && ((CharSequence)value.getValue()).length() == 0))
                {
                    requiredTerms.add(new KeyValuePair(_dictionary.getOrCreate(key),null));
                }
                else
                {
                    requiredTerms.add(new KeyValuePair(_dictionary.getOrCreate(key),value));
                }
            }

        }

        final HeadersMatcherDFAState successState =
                        new HeadersMatcherDFAState(Collections.EMPTY_MAP,Collections.singleton(result),_dictionary);

        final HeadersMatcherDFAState failState =
                        new HeadersMatcherDFAState(Collections.EMPTY_MAP,Collections.EMPTY_SET,_dictionary);

        Map<Set<KeyValuePair>, HeadersMatcherDFAState> notSeenTermsToStateMap =
                new HashMap<Set<KeyValuePair>, HeadersMatcherDFAState>();

        notSeenTermsToStateMap.put(Collections.EMPTY_SET, successState);


        final int numberOfTerms = requiredTerms.size();

        for(int numMissingTerms = 1; numMissingTerms <= numberOfTerms; numMissingTerms++)
        {
            int[] pos = new int[numMissingTerms];
            for(int i = 0; i < numMissingTerms; i++)
            {
                pos[i] = i;
            }

            final int maxTermValue = (numberOfTerms - (numMissingTerms - 1));

            while(pos[0] < maxTermValue)
            {

                Set<KeyValuePair> stateSet = new HashSet<KeyValuePair>();
                for(int posIndex = 0; posIndex < pos.length; posIndex++)
                {
                    stateSet.add(requiredTerms.get(pos[posIndex]));
                }

                final Map<HeaderKey, Map<AMQTypedValue,HeadersMatcherDFAState>> nextStateMap =
                                    new HashMap<HeaderKey, Map<AMQTypedValue,HeadersMatcherDFAState>>();


                for(int posIndex = 0; posIndex < pos.length; posIndex++)
                {
                    KeyValuePair nextTerm = requiredTerms.get(pos[posIndex]);
                    HashSet<KeyValuePair> nextStateSet =
                            new HashSet<KeyValuePair>(stateSet);
                    nextStateSet.remove(nextTerm);

                    Map<AMQTypedValue, HeadersMatcherDFAState> valueToStateMap =
                            new HashMap<AMQTypedValue, HeadersMatcherDFAState>();
                    nextStateMap.put(nextTerm._key, valueToStateMap);

                    valueToStateMap.put( nextTerm._value,notSeenTermsToStateMap.get(nextStateSet));
                    if(nextTerm._value != null)
                    {
                        valueToStateMap.put(null, failState);
                    }


                }


                HeadersMatcherDFAState newState = new HeadersMatcherDFAState(nextStateMap, Collections.EMPTY_SET, _dictionary);

                notSeenTermsToStateMap.put(stateSet, newState);

                int i = numMissingTerms;
                while(i-- != 0)
                {
                    if(++pos[i] <= numberOfTerms -(numMissingTerms-i))
                    {
                        int k = pos[i];
                        for(int j = i+1; j < numMissingTerms; j++)
                        {
                            pos[j] = ++k;
                        }
                        break;
                    }
                }
            }




        }


        return notSeenTermsToStateMap.get(new HashSet<KeyValuePair>(requiredTerms));


        
    }

    public static void main(String[] args) throws AMQFrameDecodingException
    {        

        FieldTable bindingTable = new FieldTable();

        bindingTable.setString(new AMQShortString("x-match"),"all");
        bindingTable.setInteger("a",1);
        bindingTable.setVoid(new AMQShortString("b"));
        bindingTable.setString("c","");
        bindingTable.setInteger("d",4);
        bindingTable.setInteger("e",1);



        FieldTable bindingTable2 = new FieldTable();
        bindingTable2.setString(new AMQShortString("x-match"),"all");
        bindingTable2.setInteger("a",1);
        bindingTable2.setVoid(new AMQShortString("b"));
        bindingTable2.setString("c","");
        bindingTable2.setInteger("d",4);
        bindingTable2.setInteger("e",1);
        bindingTable2.setInteger("f",1);


        FieldTable table = new FieldTable();
        table.setInteger("a",1);
        table.setInteger("b",2);
        table.setString("c","");
        table.setInteger("d",4);
        table.setInteger("e",1);
        table.setInteger("f",1);
        table.setInteger("h",1);
        table.setInteger("i",1);
        table.setInteger("j",1);
        table.setInteger("k",1);
        table.setInteger("l",1);

        org.apache.mina.common.ByteBuffer buffer = org.apache.mina.common.ByteBuffer.allocate( (int) table.getEncodedSize());
        EncodingUtils.writeFieldTableBytes(buffer, table);
        buffer.flip();

        FieldTable table2 = EncodingUtils.readFieldTable(buffer);



        FieldTable bindingTable3 = new FieldTable();
        bindingTable3.setString(new AMQShortString("x-match"),"any");
        bindingTable3.setInteger("a",1);
        bindingTable3.setInteger("b",3);


        FieldTable bindingTable4 = new FieldTable();
        bindingTable4.setString(new AMQShortString("x-match"),"any");
        bindingTable4.setVoid(new AMQShortString("a"));


        FieldTable bindingTable5 = new FieldTable();
        bindingTable5.setString(new AMQShortString("x-match"),"all");
        bindingTable5.setString(new AMQShortString("h"),"hello");

        for(int i = 0; i < 100; i++)
        {
            printMatches(new FieldTable[] {bindingTable5} , table2);
        }



    }



    private static void printMatches(final FieldTable[] bindingKeys, final FieldTable routingKey)
    {
        HeadersMatcherDFAState sm = null;
        Map<HeaderMatcherResult, String> resultMap = new HashMap<HeaderMatcherResult, String>();

        HeadersParser parser = new HeadersParser();

        for(int i = 0; i < bindingKeys.length; i++)
        {
            HeaderMatcherResult r = new HeaderMatcherResult();
            resultMap.put(r, bindingKeys[i].toString());


            if(i==0)
            {
                sm = parser.createStateMachine(bindingKeys[i], r);
            }
            else
            {
                sm = sm.mergeStateMachines(parser.createStateMachine(bindingKeys[i], r));
            }
        }

        Collection<HeaderMatcherResult> results = null;
        long beforeTime = System.currentTimeMillis();
        for(int i = 0; i < 1000000; i++)
        {
            routingKey.size();

            assert sm != null;
            results = sm.match(routingKey);

        }
        long elapsed = System.currentTimeMillis() - beforeTime;
        System.out.println("1000000 Iterations took: " + elapsed);
        Collection<String> resultStrings = new ArrayList<String>();

        assert results != null;
        for(HeaderMatcherResult result : results)
        {
            resultStrings.add(resultMap.get(result));
        }

        final ArrayList<String> nonMatches = new ArrayList<String>();
        for(FieldTable key : bindingKeys)
        {
            nonMatches.add(key.toString());
        }
        nonMatches.removeAll(resultStrings);
        System.out.println("\""+routingKey+"\" matched with " + resultStrings + " DID NOT MATCH with " + nonMatches);


    }


    public final static class KeyValuePair
    {
        public final HeaderKey _key;
        public final AMQTypedValue _value;
        private final int _hashCode;

        public KeyValuePair(final HeaderKey key, final AMQTypedValue value)
        {
            _key = key;
            _value = value;
            int hash = (1 + 31 * _key.hashCode());
            if(_value != null)
            {
                hash+=_value.hashCode();
            }
            _hashCode = hash;
        }

        public int hashCode()
        {
            return _hashCode;
        }

        public boolean equals(Object o)
        {
            KeyValuePair other = (KeyValuePair)o;
            return (_key == other._key) && (_value == null ? other._value == null : _value.equals(other._value));
        }


        public String toString()
        {
            return "{" + _key + " -> " + _value + "}";
        }

    }
}
