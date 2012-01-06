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
using System.Runtime.Serialization;
using System.Text;

namespace Apache.Qpid.Client.Qms
{
    [Serializable]
    public class UrlSyntaxException : UriFormatException
    {
        private string _url;
        private int _index;
        private int _length;

        public int GetIndex()
        {
            return _index;
        }

        public UrlSyntaxException(String input, String reason)
            : this(input, reason, -1)
        {
        }

        private UrlSyntaxException(string input, string reason, int index)
            :
            this(input, reason, index, input.Length)
        {
        }

        public UrlSyntaxException(String url, String error, int index, int length)
            : base(error)
        {
            _url = url;
            _index = index;
            _length = length;
        }

        protected UrlSyntaxException(SerializationInfo info, StreamingContext ctxt)
           : base(info, ctxt)
        {
            _url = info.GetString("Url");
            _index = info.GetInt32("Index");
            _length = info.GetInt32("Length");
        }

        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            base.GetObjectData(info, context);
            info.AddValue("Url", _url);
            info.AddValue("Index", _index);
            info.AddValue("Length", _length);
        }

        private static String getPositionString(int index, int length)
        {
            StringBuilder sb = new StringBuilder(index + 1);

            for (int i = 0; i < index; i++)
            {
                sb.Append(" ");
            }

            if (length > -1)
            {
                for (int i = 0; i < length; i++)
                {
                    sb.Append('^');
                }
            }

            return sb.ToString();
        }


        public String toString()
        {
            StringBuilder sb = new StringBuilder();

//            sb.Append(getReason());

            if (_index > -1)
            {
                if (_length != -1)
                {
                    sb.Append(" between indicies ");
                    sb.Append(_index);
                    sb.Append(" and ");
                    sb.Append(_length);
                }
                else
                {
                    sb.Append(" at index ");
                    sb.Append(_index);
                }
            }

            sb.Append(" ");
            if (_index != -1)
            {
                sb.Append("\n");
            }

            sb.Append(_url);

            if (_index != -1)
            {
                sb.Append("\n");
                sb.Append(getPositionString(_index, _length));
            }

            return sb.ToString();
        }
    }
}
