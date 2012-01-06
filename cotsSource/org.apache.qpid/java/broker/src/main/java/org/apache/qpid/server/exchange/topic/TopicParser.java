package org.apache.qpid.server.exchange.topic;

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.AMQShortStringTokenizer;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.io.IOException;

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
public class TopicParser
{
    private static final byte TOPIC_DELIMITER = (byte)'.';

    private final TopicWordDictionary _dictionary = new TopicWordDictionary();
    private final AtomicReference<TopicMatcherDFAState> _stateMachine = new AtomicReference<TopicMatcherDFAState>();

    private static class Position
    {
        private final TopicWord _word;
        private final boolean _selfTransition;
        private final int _position;
        private final boolean _endState;
        private boolean _followedByAnyLoop;


        public Position(final int position, final TopicWord word, final boolean selfTransition, final boolean endState)
        {
            _position = position;
            _word = word;
            _selfTransition = selfTransition;
            _endState = endState;
        }


    }

    private static final Position ERROR_POSITION = new Position(Integer.MAX_VALUE,null, true, false);

    private static class SimpleState
    {
        Set<Position> _positions;
        Map<TopicWord, SimpleState> _nextState;
    }


    public void addBinding(AMQShortString bindingKey, TopicMatcherResult result)
    {

        TopicMatcherDFAState startingStateMachine;
        TopicMatcherDFAState newStateMachine;

        do
        {
            startingStateMachine = _stateMachine.get();
            if(startingStateMachine == null)
            {
                newStateMachine = createStateMachine(bindingKey, result);
            }
            else
            {
                newStateMachine = startingStateMachine.mergeStateMachines(createStateMachine(bindingKey, result));
            }

        }
        while(!_stateMachine.compareAndSet(startingStateMachine,newStateMachine));

    }

    public Collection<TopicMatcherResult> parse(AMQShortString routingKey)
    {
        TopicMatcherDFAState stateMachine = _stateMachine.get();
        if(stateMachine == null)
        {
            return Collections.EMPTY_SET;
        }
        else
        {
            return stateMachine.parse(_dictionary,routingKey);
        }
    }


    TopicMatcherDFAState createStateMachine(AMQShortString bindingKey, TopicMatcherResult result)
    {
        List<TopicWord> wordList = createTopicWordList(bindingKey);
        int wildCards = 0;
        for(TopicWord word : wordList)
        {
            if(word == TopicWord.WILDCARD_WORD)
            {
                wildCards++;
            }
        }
        if(wildCards == 0)
        {
            TopicMatcherDFAState[] states = new TopicMatcherDFAState[wordList.size()+1];
            states[states.length-1] = new TopicMatcherDFAState(Collections.EMPTY_MAP, Collections.singleton(result));
            for(int i = states.length-2; i >= 0; i--)
            {
                states[i] = new TopicMatcherDFAState(Collections.singletonMap(wordList.get(i),states[i+1]),Collections.EMPTY_SET);

            }
            return states[0];
        }
        else if(wildCards == wordList.size())
        {
            Map<TopicWord,TopicMatcherDFAState> stateMap = new HashMap<TopicWord,TopicMatcherDFAState>();
            TopicMatcherDFAState state = new TopicMatcherDFAState(stateMap, Collections.singleton(result));
            stateMap.put(TopicWord.ANY_WORD, state);
            return state;
        }


        int positionCount = wordList.size() - wildCards;

        Position[] positions = new Position[positionCount+1];

        int lastWord;

        if(wordList.get(wordList.size()-1)== TopicWord.WILDCARD_WORD)
        {
            lastWord = wordList.size()-1;
            positions[positionCount] = new Position(positionCount, TopicWord.ANY_WORD, true, true);
        }
        else
        {
            lastWord = wordList.size();
            positions[positionCount] = new Position(positionCount, TopicWord.ANY_WORD, false, true);
        }


        int pos = 0;
        int wordPos = 0;


        while(wordPos < lastWord)
        {
            TopicWord word = wordList.get(wordPos++);

            if(word == TopicWord.WILDCARD_WORD)
            {
                int nextWordPos = wordPos++;
                word = wordList.get(nextWordPos);

                positions[pos] = new Position(pos++,word,true,false);
            }
            else
            {
                positions[pos] = new Position(pos++,word,false,false);
            }

        }


        for(int p = 0; p<positionCount; p++)
        {
            boolean followedByWildcards = true;

            int n = p;
            while(followedByWildcards && n<(positionCount+1))
            {

                if(positions[n]._selfTransition)
                {
                    break;
                }
                else if(positions[n]._word!=TopicWord.ANY_WORD)
                {
                    followedByWildcards = false;
                }
                n++;
            }


            positions[p]._followedByAnyLoop = followedByWildcards && (n!= positionCount+1);
        }


        // from each position you transition to a set of other positions.
        // we approach this by examining steps of increasing length - so we
        // look how far we can go from the start position in 1 word, 2 words, etc...

        Map<Set<Position>,SimpleState> stateMap = new HashMap<Set<Position>,SimpleState>();


        SimpleState state = new SimpleState();
        state._positions = Collections.singleton( positions[0] );
        stateMap.put(state._positions, state);

        calculateNextStates(state, stateMap, positions);

        SimpleState[] simpleStates = stateMap.values().toArray(new SimpleState[stateMap.size()]);
        HashMap<TopicWord, TopicMatcherDFAState>[] dfaStateMaps = new HashMap[simpleStates.length];
        Map<SimpleState, TopicMatcherDFAState> simple2DFAMap = new HashMap<SimpleState, TopicMatcherDFAState>();

        for(int i = 0; i < simpleStates.length; i++)
        {

            Collection<TopicMatcherResult> results;
            boolean endState = false;

            for(Position p : simpleStates[i]._positions)
            {
                if(p._endState)
                {
                    endState = true;
                    break;
                }
            }

            if(endState)
            {
                results = Collections.singleton(result);
            }
            else
            {
                results = Collections.EMPTY_SET;
            }

            dfaStateMaps[i] = new HashMap<TopicWord, TopicMatcherDFAState>();
            simple2DFAMap.put(simpleStates[i], new TopicMatcherDFAState(dfaStateMaps[i],results));

        }
        for(int i = 0; i < simpleStates.length; i++)
        {
            SimpleState simpleState = simpleStates[i];

            Map<TopicWord, SimpleState> nextSimpleStateMap = simpleState._nextState;
            for(Map.Entry<TopicWord, SimpleState> stateMapEntry : nextSimpleStateMap.entrySet())
            {
                dfaStateMaps[i].put(stateMapEntry.getKey(), simple2DFAMap.get(stateMapEntry.getValue()));
            }

        }

        return simple2DFAMap.get(state);

    }



    private void calculateNextStates(final SimpleState state,
                                     final Map<Set<Position>, SimpleState> stateMap,
                                     final Position[] positions)
    {
        Map<TopicWord, Set<Position>> transitions = new HashMap<TopicWord,Set<Position>>();

        for(Position pos : state._positions)
        {
            if(pos._selfTransition)
            {
                Set<Position> dest = transitions.get(TopicWord.ANY_WORD);
                if(dest == null)
                {
                    dest = new HashSet<Position>();
                    transitions.put(TopicWord.ANY_WORD,dest);
                }
                dest.add(pos);
            }

            final int nextPos = pos._position + 1;
            Position nextPosition = nextPos == positions.length ? ERROR_POSITION : positions[nextPos];

            Set<Position> dest = transitions.get(pos._word);
            if(dest == null)
            {
                dest = new HashSet<Position>();
                transitions.put(pos._word,dest);
            }
            dest.add(nextPosition);

        }

        Set<Position> anyWordTransitions = transitions.get(TopicWord.ANY_WORD);
        if(anyWordTransitions != null)
        {
            for(Set<Position> dest : transitions.values())
            {
                dest.addAll(anyWordTransitions);
            }
        }

        state._nextState = new HashMap<TopicWord, SimpleState>();

        for(Map.Entry<TopicWord,Set<Position>> dest : transitions.entrySet())
        {

            if(dest.getValue().size()>1)
            {
                dest.getValue().remove(ERROR_POSITION);
            }
            Position loopingTerminal = null;
            for(Position destPos : dest.getValue())
            {
                if(destPos._selfTransition && destPos._endState)
                {
                    loopingTerminal = destPos;
                    break;
                }
            }

            if(loopingTerminal!=null)
            {
                dest.setValue(Collections.singleton(loopingTerminal));
            }
            else
            {
                Position anyLoop = null;
                for(Position destPos : dest.getValue())
                {
                    if(destPos._followedByAnyLoop)
                    {
                        if(anyLoop == null || anyLoop._position<destPos._position)
                        {
                            anyLoop = destPos;
                        }
                    }
                }
                if(anyLoop != null)
                {
                    Collection<Position> removals = new ArrayList<Position>();
                    for(Position destPos : dest.getValue())
                    {
                        if(destPos._position < anyLoop._position)
                        {
                            removals.add(destPos);
                        }
                    }
                    dest.getValue().removeAll(removals);
                }
            }

            SimpleState stateForEntry = stateMap.get(dest.getValue());
            if(stateForEntry == null)
            {
                stateForEntry = new SimpleState();
                stateForEntry._positions = dest.getValue();
                stateMap.put(dest.getValue(),stateForEntry);
                calculateNextStates(stateForEntry,
                                    stateMap,
                                    positions);
            }
            state._nextState.put(dest.getKey(),stateForEntry);



        }

        // remove redundant transitions
        SimpleState anyWordState = state._nextState.get(TopicWord.ANY_WORD);
        if(anyWordState != null)
        {
            List<TopicWord> removeList = new ArrayList<TopicWord>();
            for(Map.Entry<TopicWord,SimpleState> entry : state._nextState.entrySet())
            {
                if(entry.getValue() == anyWordState && entry.getKey() != TopicWord.ANY_WORD)
                {
                    removeList.add(entry.getKey());
                }
            }
            for(TopicWord removeKey : removeList)
            {
                state._nextState.remove(removeKey);
            }
        }


    }

    private List<TopicWord> createTopicWordList(final AMQShortString bindingKey)
    {
        AMQShortStringTokenizer tokens = bindingKey.tokenize(TOPIC_DELIMITER);
        TopicWord previousWord = null;

        List<TopicWord> wordList = new ArrayList<TopicWord>();

        while(tokens.hasMoreTokens())
        {
            TopicWord nextWord = _dictionary.getOrCreateWord(tokens.nextToken());
            if(previousWord == TopicWord.WILDCARD_WORD)
            {

                if(nextWord == TopicWord.WILDCARD_WORD)
                {
                    // consecutive wildcards can be merged
                    // i.e. subsequent wildcards can be discarded
                    continue;
                }
                else if(nextWord == TopicWord.ANY_WORD)
                {
                    // wildcard and anyword can be reordered to always put anyword first
                    wordList.set(wordList.size()-1,TopicWord.ANY_WORD);
                    nextWord = TopicWord.WILDCARD_WORD;
                }
            }
            wordList.add(nextWord);
            previousWord = nextWord;

        }
        return wordList;
    }


    public static void main(String[] args)
    {

        printMatches("#.b.*.*.*.*.*.h.#.j.*.*.*.*.*.*.q.#.r.*.*.*.*.*.*.*.*","a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z");
        printMatches(new String[]{
                        "#.a.#",
                        "#.b.#",
                        "#.c.#",
                        "#.d.#",
                        "#.e.#",
                        "#.f.#",
                        "#.g.#",
                        "#.h.#",
                        "#.i.#",
                        "#.j.#",
                        "#.k.#",
                        "#.l.#",
                        "#.m.#",
                        "#.n.#",
                        "#.o.#",
                        "#.p.#",
                        "#.q.#"

        }, "a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z");        
/*
        printMatches(new String[]{
                "#.a.#",
                "#.b.#",
                "#.c.#",
                "#.d.#",
                "#.e.#",
                "#.f.#",
                "#.g.#",
                "#.h.#",
                "#.i.#",
                "#.j.#",
                "#.k.#",
                "#.l.#",
                "#.m.#",
                "#.n.#",
                "#.o.#",
                "#.p.#",
                "#.q.#",
                "#.r.#",
                "#.s.#",
                "#.t.#",
                "#.u.#",
                "#.v.#",
                "#.w.#",
                "#.x.#",
                "#.y.#",
                "#.z.#"


        },"a.b");

        printMatches("#.b.*.*.*.*.*.h.#.j.*.*.*.*.*.p.#.r.*.*.*.*.*","a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z");
        printMatches("#.b.*.*.*.*.*.h.#.j.*.*.*.*.*.p.#.r.*.*.*.*.*.*.*.*","a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z");
        printMatches("a.#.b.#","a.b.b.b.b.b.b.b.c");

*/

        printMatches("","");
        printMatches("a","a");
        printMatches("a","");
        printMatches("","a");
        printMatches("a.b","a.b");
        printMatches("a","a.b");
        printMatches("a.b","a");
        printMatches("*","a");
        printMatches("*.b","a.b");
        printMatches("*.*","a.b");
        printMatches("a.*","a.b");
        printMatches("a.*.#","a.b");
        printMatches("a.#.b","a.b");

        printMatches("#.b","a");
        printMatches("#.b","a.b");
        printMatches("#.a.b","a.b");


        printMatches("#","");
        printMatches("#","a");
        printMatches("#","a.b");
        printMatches("#.#","a.b");
        printMatches("#.*","a.b");

        printMatches("#.a.b","a.b");
        printMatches("a.b.#","a.b");
        printMatches("a.#","a.b");
        printMatches("#.*.#","a.b");
        printMatches("#.*.b.#","a.b");
        printMatches("#.a.*.#","a.b");
        printMatches("#.a.#.b.#","a.b");
        printMatches("#.*.#.*.#","a.b");
        printMatches("*.#.*.#","a.b");
        printMatches("#.*.#.*","a.b");


        printMatches(new String[]{"a.#.b.#","a.*.#.b.#"},"a.b.b.b.b.b.b.b.c");


        printMatches(new String[]{"a.b", "a.c"},"a.b");
        printMatches(new String[]{"a.#", "a.c", "#.b"},"a.b");
        printMatches(new String[]{"a.#", "a.c", "#.b", "#", "*.*"},"a.b");

        printMatches(new String[]{"a.b.c.d.e.#", "a.b.c.d.#", "a.b.c.d.*", "a.b.c.#", "#.e", "a.*.c.d.e","#.c.*.#.*.*"},"a.b.c.d.e");
        printMatches(new String[]{"a.b.c.d.e.#", "a.b.c.d.#", "a.b.c.d.*", "a.b.c.#", "#.e", "a.*.c.d.e","#.c.*.#.*.*"},"a.b.c.d.f.g");




    }

    private static void printMatches(final String[] bindingKeys, final String routingKey)
    {
        TopicMatcherDFAState sm = null;
        Map<TopicMatcherResult, String> resultMap = new HashMap<TopicMatcherResult, String>();

        TopicParser parser = new TopicParser();

        long start = System.currentTimeMillis();
        for(int i = 0; i < bindingKeys.length; i++)
        {
            System.out.println((System.currentTimeMillis() - start) + ":\t" + bindingKeys[i]);
            TopicMatcherResult r = new TopicMatcherResult(){};
            resultMap.put(r, bindingKeys[i]);
            AMQShortString bindingKeyShortString = new AMQShortString(bindingKeys[i]);

            System.err.println("=====================================================");
            System.err.println("Adding binding key: " + bindingKeyShortString);
            System.err.println("-----------------------------------------------------");


            if(i==0)
            {
                sm = parser.createStateMachine(bindingKeyShortString, r);
            }
            else
            {
                sm = sm.mergeStateMachines(parser.createStateMachine(bindingKeyShortString, r));
            }
            System.err.println(sm.reachableStates());
            System.err.println("=====================================================");
            try
            {
                System.in.read();
            }
            catch (IOException e)
            {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
        AMQShortString routingKeyShortString = new AMQShortString(routingKey);

        Collection<TopicMatcherResult> results = sm.parse(parser._dictionary, routingKeyShortString);
        Collection<String> resultStrings = new ArrayList<String>();

        for(TopicMatcherResult result : results)
        {
            resultStrings.add(resultMap.get(result));
        }

        final ArrayList<String> nonMatches = new ArrayList<String>(Arrays.asList(bindingKeys));
        nonMatches.removeAll(resultStrings);
        System.out.println("\""+routingKeyShortString+"\" matched with " + resultStrings + " DID NOT MATCH with " + nonMatches);


    }

    private static void printMatches(String bindingKey, String routingKey)
    {
        printMatches(new String[] { bindingKey }, routingKey);
    }


    private static boolean matches(String bindingKey, String routingKey)
    {
        AMQShortString bindingKeyShortString = new AMQShortString(bindingKey);
        AMQShortString routingKeyShortString = new AMQShortString(routingKey);
        TopicParser parser = new TopicParser();

        final TopicMatcherResult result = new TopicMatcherResult(){};

        TopicMatcherDFAState sm = parser.createStateMachine(bindingKeyShortString, result);
        return !sm.parse(parser._dictionary,routingKeyShortString).isEmpty();

    }

}
