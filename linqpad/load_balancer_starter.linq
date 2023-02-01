<Query Kind="Statements" />

// #1: Each server is healthy or not?
// #2: Hash mac addresses into hash table, with the value being the IP/machine
// #3: There should be a reverse lookup to determine machine distribution.
// #4: There should be a mechanism to ensure that distribution is within set limit (30-30-30)
// #5: There should be a mechanism to re-hash when the machine count changes.

// Questions the code needs to ask:
// #1: How many machines are there?
// #2: For a given machine is it healthy?
// #3: When a node is added, how many machines need to change their stickpoint?
// #4: When a node is removed, how many existing machines are impacted, and 
// how many will have to change their stickpoint?
// #5: What is the number of nodes allocated to each machine?

var innerHashTable = new Hashtable();
var hashtable = Hashtable.Synchronized(innerHashTable);

innerHashTable.Add("something", "another thing");

Console.WriteLine(hashtable);