
(10:07) start the session with Valerio's question about difference between Datum and Redeemer
    Roberto explain the datum is similar to how data bases handles passwords. To vefify password without storing it, uses hash.
    Datum was just tha hash stored into the UTxO but after Basil fork the Data 

(20:10) Roberto is checking how do the Datum get validated.

(23:50) Review on ChainIndexTxOut

(30:00) Two tips about Datum and Redeemer
    When you are created a UTxO 
    the spending transaction always have to include the Datum of the input to spend.
    The producing transaction can avoid including the datum

(33:38) Jump into VestingContract code example to finish the off chain part of the code