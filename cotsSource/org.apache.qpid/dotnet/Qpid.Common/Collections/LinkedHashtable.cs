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
using System;
using System.Collections;

namespace Apache.Qpid.Collections
{
    public class LinkedHashtable : IDictionary
    {
        /// <summary>
        /// Maps from key to LinkedDictionaryEntry        
        /// </summary>
        private Hashtable _indexedValues = new Hashtable();

        private LinkedDictionaryEntry _head;

        private LinkedDictionaryEntry _tail;

        private class LinkedDictionaryEntry
        {
            public LinkedDictionaryEntry _previous;
            public LinkedDictionaryEntry _next;
            internal DictionaryEntry _entry;

            public LinkedDictionaryEntry(object key, object value)
            {
                _entry = new DictionaryEntry(key, value);
            }
        }

        public object this[object key]
        {
            get
            {
                LinkedDictionaryEntry entry = (LinkedDictionaryEntry)_indexedValues[key];
                if (entry == null)
                {
                    return null; // key not found
                }
                else
                {
                    return entry._entry.Value;
                }
            }

            set
            {
                LinkedDictionaryEntry entry = (LinkedDictionaryEntry)_indexedValues[key];
                if (entry == null)
                {
                    Add(key, value);
                }
                else
                {
                    entry._entry.Value = value;
                }
            }
        }

        /// <summary>
        /// Collect keys in linked order.
        /// </summary>
        public ICollection Keys
        {
            get
            {
                IList result = new ArrayList();
                foreach (DictionaryEntry entry in this)
                {
                    result.Add(entry.Key);
                }
                return result;
            }
        }

        /// <summary>
        /// Collect values in linked order.
        /// </summary>
        public ICollection Values
        {
            get
            {
                IList result = new ArrayList();
                foreach (DictionaryEntry entry in this)
                {
                    result.Add(entry.Value);
                }
                return result;
            }
        }

        public bool IsReadOnly
        {
            get { return _indexedValues.IsReadOnly; }
        }

        public bool IsFixedSize
        {
            get { return _indexedValues.IsFixedSize; }
        }

        public bool Contains(object key)
        {
            return _indexedValues.Contains(key);
        }

        public void Add(object key, object value)
        {
            if (key == null) throw new ArgumentNullException("key");
            
            if (Contains(key))
            {
                throw new ArgumentException("LinkedHashtable already contains key. key=" + key);
            }
            
            LinkedDictionaryEntry de = new LinkedDictionaryEntry(key, value);
            if (_head == null)
            {
                _head = de;
                _tail = de;
            }
            else
            {
                _tail._next = de;
                de._previous = _tail;
                _tail = de;
            }
            _indexedValues[key] = de;
        }

        public void Clear()
        {
            _indexedValues.Clear();
        }

        IDictionaryEnumerator IDictionary.GetEnumerator()
        {
            return new LHTEnumerator(this);
        }

        public void Remove(object key)
        {
            if (key == null) throw new ArgumentNullException("key");
            
            LinkedDictionaryEntry de = (LinkedDictionaryEntry)_indexedValues[key];
            if (de == null) return; // key not found.
            LinkedDictionaryEntry prev = de._previous;
            if (prev == null)
            {
                _head = de._next;
            }
            else
            {
                prev._next = de._next;
            }

            LinkedDictionaryEntry next = de._next;
            if (next == null)
            {
                _tail = de;
            }
            else
            {
                next._previous = de._previous;
            }
        }
        
        private LinkedDictionaryEntry Head
        {
            get
            {
                return _head;
            }
        }

//        private LinkedDictionaryEntry Tail
//        {
//            get
//            {
//                return _tail;
//            }
//        }

        private class LHTEnumerator : IDictionaryEnumerator
        {
            private LinkedHashtable _container;

            private LinkedDictionaryEntry _current;

            /// <summary>
            /// Set once we have navigated off the end of the collection
            /// </summary>
            private bool _needsReset = false;

            public LHTEnumerator(LinkedHashtable container)
            {
                _container = container;                
            }

            public object Current
            {
                get
                {
                    if (_current == null)
                    {
                        throw new Exception("Iterator before first element");
                    }
                    else
                    {
                        return _current._entry;
                    }
                }
            }

            public object Key
            {
                get { return _current._entry.Key; }
            }

            public object Value
            {
                get { return _current._entry.Value; }
            }

            public DictionaryEntry Entry
            {
                get
                {
                    return _current._entry;
                }
            }

            public bool MoveNext()
            {
                if (_needsReset)
                {
                    return false;
                }
                else if (_current == null)
                {
                    _current = _container.Head;                    
                }
                else
                {
                    _current = _current._next;
                }
                _needsReset = (_current == null);
                return !_needsReset;                
            }

            public void Reset()
            {
                _current = null;
                _needsReset = false;
            }
        }

        public void MoveToHead(object key)
        {
            LinkedDictionaryEntry de = (LinkedDictionaryEntry)_indexedValues[key];
            if (de == null)
            {
                throw new ArgumentException("Key " + key + " not found");
            }
            // if the head is the element then there is nothing to do
            if (_head == de)
            {
                return;
            }
            de._previous._next = de._next;
            if (de._next != null)
            {
                de._next._previous = de._previous;
            }
            else
            {
                _tail = de._previous;
            }
            de._next = _head;
            _head = de;
            de._previous = null;
        }

        public void CopyTo(Array array, int index)
        {
            _indexedValues.CopyTo(array, index);
        }

        public int Count
        {
            get { return _indexedValues.Count; }
        }

        public object SyncRoot
        {
            get { return _indexedValues.SyncRoot; }
        }

        public bool IsSynchronized
        {
            get { return _indexedValues.IsSynchronized; }
        }

        public IEnumerator GetEnumerator()
        {
            return new LHTEnumerator(this);
        }
    }
}
