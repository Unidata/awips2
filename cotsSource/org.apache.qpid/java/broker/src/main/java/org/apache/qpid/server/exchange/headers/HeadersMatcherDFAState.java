package org.apache.qpid.server.exchange.headers;

import org.apache.qpid.framing.AMQTypedValue;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.server.exchange.topic.TopicMatcherDFAState;
import org.apache.qpid.server.exchange.topic.TopicWord;
import org.apache.qpid.server.exchange.topic.TopicMatcherResult;

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
public class HeadersMatcherDFAState
{


    private final Collection<HeaderMatcherResult> _results;
    private final Map<HeaderKey, Map<AMQTypedValue,HeadersMatcherDFAState>> _nextStateMap;
    private final HeaderKeyDictionary _dictionary;

    public HeadersMatcherDFAState(Map<HeaderKey, Map<AMQTypedValue,HeadersMatcherDFAState>> nextStateMap,
                                Collection<HeaderMatcherResult> results,
                                HeaderKeyDictionary dictionary)
    {
        _nextStateMap = nextStateMap;
        _results = results;
        _dictionary = dictionary;
    }


    public Collection<HeaderMatcherResult> match(final FieldTable table)
    {
        return match(table.iterator());
    }



    public Collection<HeaderMatcherResult> match(Iterator<Map.Entry<AMQShortString,AMQTypedValue>> fieldTableIterator)
    {

        if(_nextStateMap.isEmpty())
        {
            return _results;
        }

        while(fieldTableIterator.hasNext())
        {

            Map.Entry<AMQShortString, AMQTypedValue> fieldTableEntry = fieldTableIterator.next();
            HeaderKey key = _dictionary.get(fieldTableEntry.getKey());
            if(key != HeaderKey.UNKNOWN)
            {
                Map<AMQTypedValue, HeadersMatcherDFAState> valueToStateMap = _nextStateMap.get(key);

                if(valueToStateMap != null)
                {
                    HeadersMatcherDFAState nextState = valueToStateMap.get(fieldTableEntry.getValue());

                    if(nextState == null)
                    {
                        nextState = valueToStateMap.get(null);
                    }
                    if(nextState != null && nextState != this)
                    {
                        return nextState.match(fieldTableIterator);
                    }
                }

            }
        }

        return _results;

    }


    HeadersMatcherDFAState mergeStateMachines(HeadersMatcherDFAState otherStateMachine)
    {

        assert(otherStateMachine._dictionary == _dictionary);

        Map<Set<HeadersMatcherDFAState>, HeadersMatcherDFAState> newStateMap= new HashMap<Set<HeadersMatcherDFAState>, HeadersMatcherDFAState>();

        Collection<HeaderMatcherResult> results;

        if(_results.isEmpty())
        {
            results = otherStateMachine._results;
        }
        else if(otherStateMachine._results.isEmpty())
        {
            results = _results;
        }
        else
        {
            results = new HashSet<HeaderMatcherResult>(_results);
            results.addAll(otherStateMachine._results);
        }


        final Map<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>> newNextStateMap = new HashMap<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>>();

        HeadersMatcherDFAState newState = new HeadersMatcherDFAState(newNextStateMap, results, _dictionary);


        Set<HeadersMatcherDFAState> oldStates = new HashSet<HeadersMatcherDFAState>();
        oldStates.add(this);
        oldStates.add(otherStateMachine);

        newStateMap.put(oldStates, newState);

        mergeStateMachines(oldStates, newNextStateMap, newStateMap);

        return newState;


    }

    private void mergeStateMachines(final Set<HeadersMatcherDFAState> oldStates,
                                    final Map<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>> newNextStateMap,
                                    final Map<Set<HeadersMatcherDFAState>, HeadersMatcherDFAState> newStateMap)
    {
        Map<HeaderKey, Map<AMQTypedValue, Set<HeadersMatcherDFAState>>> nfaMap = new HashMap<HeaderKey, Map<AMQTypedValue, Set<HeadersMatcherDFAState>>>();

        Set<HeaderKey> distinctKeys = new HashSet<HeaderKey>();

        for(HeadersMatcherDFAState state : oldStates)
        {
            Map<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>> map = state._nextStateMap;

            for(Map.Entry<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>> entry : map.entrySet())
            {
                Map<AMQTypedValue, Set<HeadersMatcherDFAState>> valueToStatesMap = nfaMap.get(entry.getKey());

                if(valueToStatesMap == null)
                {
                    valueToStatesMap = new HashMap<AMQTypedValue, Set<HeadersMatcherDFAState>>();
                    nfaMap.put(entry.getKey(), valueToStatesMap);
                }

                for(Map.Entry<AMQTypedValue, HeadersMatcherDFAState> valueToStateEntry : entry.getValue().entrySet())
                {
                    Set<HeadersMatcherDFAState> states = valueToStatesMap.get(valueToStateEntry.getKey());
                    if(states == null)
                    {
                        states = new HashSet<HeadersMatcherDFAState>();
                        valueToStatesMap.put(valueToStateEntry.getKey(),states);
                    }
                    states.add(valueToStateEntry.getValue());
                }

                distinctKeys.add(entry.getKey());
            }
        }

        Map<HeaderKey, Set<HeadersMatcherDFAState>> anyValueStates = new HashMap<HeaderKey, Set<HeadersMatcherDFAState>>();

        for(HeaderKey distinctKey : distinctKeys)
        {
            Map<AMQTypedValue, Set<HeadersMatcherDFAState>> valueToStateMap = nfaMap.get(distinctKey);
            if(valueToStateMap != null)
            {
                Set<HeadersMatcherDFAState> statesForKeyDefault = valueToStateMap.get(null);
                if(statesForKeyDefault != null)
                {
                    anyValueStates.put(distinctKey,  statesForKeyDefault);
                }
            }
        }

        // add the defaults for "null" to all other specified values of a given header key

        for( Map.Entry<HeaderKey,Map<AMQTypedValue,Set<HeadersMatcherDFAState>>> entry : nfaMap.entrySet())
        {
            Map<AMQTypedValue, Set<HeadersMatcherDFAState>> valueToStatesMap = entry.getValue();
            for(Map.Entry<AMQTypedValue, Set<HeadersMatcherDFAState>> valueToStates : valueToStatesMap.entrySet())
            {
                if(valueToStates.getKey() != null)
                {


                    Set<HeadersMatcherDFAState> defaults = anyValueStates.get(entry.getKey());
                    if(defaults != null)
                    {
                        valueToStates.getValue().addAll(defaults);
                    }
                }
            }
        }

        // if a given header key is not mentioned in the map of a machine; then that machine would stay at the same state
        // for that key.
        for(HeaderKey distinctKey : distinctKeys)
        {
            Map<AMQTypedValue, Set<HeadersMatcherDFAState>> valueToStatesMap = nfaMap.get(distinctKey);
            for(HeadersMatcherDFAState oldState : oldStates)
            {
                if(!oldState._nextStateMap.containsKey(distinctKey))
                {
                    for(Set<HeadersMatcherDFAState> endStates : valueToStatesMap.values())
                    {
                        endStates.add(oldState);
                    }
                }
            }
        }




        for(Map.Entry<HeaderKey,Map<AMQTypedValue,Set<HeadersMatcherDFAState>>> transitionClass : nfaMap.entrySet())
        {
            Map<AMQTypedValue, HeadersMatcherDFAState> valueToDFAState = newNextStateMap.get(transitionClass.getKey());
            if(valueToDFAState == null)
            {
                valueToDFAState = new HashMap<AMQTypedValue, HeadersMatcherDFAState>();
                newNextStateMap.put(transitionClass.getKey(), valueToDFAState);
            }

            for(Map.Entry<AMQTypedValue,Set<HeadersMatcherDFAState>> transition : transitionClass.getValue().entrySet())
            {
                Set<HeadersMatcherDFAState> destinations = transition.getValue();


                HeadersMatcherDFAState nextState = newStateMap.get(destinations);

                if(nextState == null)
                {

                    if(destinations.size() == 1)
                    {
                        nextState = destinations.iterator().next();
                        newStateMap.put(destinations, nextState);
                    }
                    else
                    {
                        Collection<HeaderMatcherResult> results;

                        Set<Collection<HeaderMatcherResult>> resultSets = new HashSet<Collection<HeaderMatcherResult>>();
                        for(HeadersMatcherDFAState destination : destinations)
                        {
                            resultSets.add(destination._results);
                        }
                        resultSets.remove(Collections.EMPTY_SET);
                        if(resultSets.size() == 0)
                        {
                            results = Collections.EMPTY_SET;
                        }
                        else if(resultSets.size() == 1)
                        {
                            results = resultSets.iterator().next();
                        }
                        else
                        {
                            results = new HashSet<HeaderMatcherResult>();
                            for(Collection<HeaderMatcherResult> oldResult : resultSets)
                            {
                                results.addAll(oldResult);
                            }
                        }

                        final Map<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>> nextStateMap = new HashMap<HeaderKey, Map<AMQTypedValue, HeadersMatcherDFAState>>();

                        nextState = new HeadersMatcherDFAState(nextStateMap, results, _dictionary);
                        newStateMap.put(destinations, nextState);

                        mergeStateMachines(
                                destinations,
                                           nextStateMap,
                                           newStateMap);


                    }


                }
                valueToDFAState.put(transition.getKey(),nextState);
            }
        }



        final ArrayList<HeaderKey> removeKeyList = new ArrayList<HeaderKey>();

        for(Map.Entry<HeaderKey,Map<AMQTypedValue,HeadersMatcherDFAState>> entry : _nextStateMap.entrySet())
        {
            final ArrayList<AMQTypedValue> removeValueList = new ArrayList<AMQTypedValue>();

            for(Map.Entry<AMQTypedValue,HeadersMatcherDFAState> valueToDFAState : entry.getValue().entrySet())
            {
                if(valueToDFAState.getValue() == this)
                {
                    HeadersMatcherDFAState defaultState = entry.getValue().get(null);
                    if(defaultState == null || defaultState == this)
                    {
                        removeValueList.add(valueToDFAState.getKey());
                    }
                }
            }

            for(AMQTypedValue removeValue : removeValueList)
            {
                entry.getValue().remove(removeValue);
            }

            if(entry.getValue().isEmpty())
            {
                removeKeyList.add(entry.getKey());
            }

        }

        for(HeaderKey removeKey : removeKeyList)
        {
            _nextStateMap.remove(removeKey);
        }

    }

}
