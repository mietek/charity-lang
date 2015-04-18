
data Eid A -> C = eid: A -> C.     % THE (EAGER) IDENTITY DATATYPE

data C -> Lid A = lid: C -> A.     % THE (LAZY)  IDENTITY DATATYPE

