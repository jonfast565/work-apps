with Ada.Strings.Unbounded;

package LinkedList is
   
   type Node;
   type Node_Access is access Node;
   
   type Node is record 
      Value: Ada.Strings.Unbounded.Unbounded_String;
      Next: Node_Access;
   end record;
   
   procedure Print(List: Node_Access);
   procedure Insert(List: Node_Access; Node: Node_Access);
   procedure Insert(List : Node_Access; Index : Integer; String : Ada.Strings.Unbounded.Unbounded_String);
   procedure Update(List: Node_Access; Index: Integer; Value_String: Ada.Strings.Unbounded.Unbounded_String);
   function Delete(List: Node_Access; Index: Integer) return Node_Access;

end LinkedList;
