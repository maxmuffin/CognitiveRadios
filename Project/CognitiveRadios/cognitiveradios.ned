//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see http://www.gnu.org/licenses/.
// 

//
// TODO auto-generated type
//
network Cognitiveradios
{
    @display("bgb=638,293");
    submodules:
        SecondarySource: Source {
            @display("p=105,191");

            jobPriority = 2;
            jobName = "Secondary";

        }
        PrimarySource: Source {
            @display("p=105,70");

            jobPriority = 1;
            jobName = "Primary";
        }
        sink: Sink {
            @display("p=538,127");

        }
        SecondaryQueue: PassiveQueue {
            @display("p=271,191");
            //sendingAlgorithm = "priority";

        }
        PrimaryQueue: PassiveQueue {
            @display("p=271,70");
            //sendingAlgorithm = "priority";
        }
        server: Server {
            @display("p=397,127");
            fetchingAlgorithm = "priority";

        }
    connections:

        SecondarySource.out --> SecondaryQueue.in++;
        PrimarySource.out --> PrimaryQueue.in++;
        PrimaryQueue.out++ --> server.in++;
        SecondaryQueue.out++ --> server.in++;
        server.out --> sink.in++;
}
