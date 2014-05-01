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
using System.Threading;

namespace common.org.apache.qpid.transport.util
{
    public class CircularBuffer<T>
    {
        private readonly T[] buffer;
        private Int32 nrp, nwp;
        private readonly Int32 len;
        private Int32 countValue;
        private readonly Int32 add;


        /// <summary>
        /// Constructor creates N=len element 
        /// Circular Buffer that olds MemoryStream
        /// </summary>
        public CircularBuffer(Int32 len)
        {
            buffer = new T[len];
            this.len = len;
            add = 1 - len;
            nrp = 0;
            nwp = 0;
            countValue = 0;
        }


        public void Enqueue(T t)
        {
            lock (this)
            {
                if (countValue >= (len - 1))
                {
                    // wait for room to be available                   
                            Monitor.Wait(this);                    
                }
                bool notifyDequeue = countValue <= 0;
                Load(t);
                if (notifyDequeue) //notifyDequeue)
                {
                    Monitor.PulseAll(this);
                }
            }
        }


        public T Dequeue()
        {
            lock (this)
            {
                if (countValue <= 0)
                {                   
                        Monitor.Wait(this);                  
                }
                bool notifyEnqueue = countValue >= (len - 1);
                T temp = Get();
                if (notifyEnqueue) //notifyEnqueue)
                {
                        Monitor.PulseAll(this);               
                }
                return temp;
            }
        }

        public void Close()
        {
            nrp = 0;
            nwp = 0;
            countValue = 0;
            Array.Clear(buffer, 0, len);
            lock (this)
            {
                Monitor.PulseAll(this);
            }
        }

        #region Private Support Functions

        private void Load(T t)
        {
            Int32 i = nwp;
            buffer[i] = t;
            i += add;
            if (i < 0) i += len;
            nwp = i;
            UpdateCount();
        }

        private void UpdateCount()
        {
            countValue = nwp - nrp;
            if (countValue <= 0 )
                countValue += len; // modulo buffer size			
        }

        private T Get()
        {
            Int32 i = nrp;
            T temp = buffer[i];           
            i += add;
            if (i < 0) i += len;
            nrp = i;
            countValue--;    
            return (temp);
        }

        #endregion
    }
}
