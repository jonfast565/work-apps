with LinkedList; use LinkedList;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

procedure Main is

begin
   declare
      node1: Node_Access;
      node2: Node_Access;
   begin
      node1 := new LinkedList.Node'(Value => Ada.Strings.Unbounded.To_Unbounded_String("Something"), Next => null);
      node2 := new LinkedList.Node'(Value => Ada.Strings.Unbounded.To_Unbounded_String("Another Thing"), Next => node1);
      LinkedList.Print(node2);

      LinkedList.Insert(node2, 2, Ada.Strings.Unbounded.To_Unbounded_String("Insert"));
      LinkedList.Print(node2);

      node2 := LinkedList.Delete(node2, 0);
   end;
end Main;
