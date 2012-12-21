PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE objlink (src int, dst int, dir int);
INSERT INTO "objlink" VALUES(785,775,12);
INSERT INTO "objlink" VALUES(775,779,2);
INSERT INTO "objlink" VALUES(780,779,11);
INSERT INTO "objlink" VALUES(780,779,8);
INSERT INTO "objlink" VALUES(779,1640,2);
INSERT INTO "objlink" VALUES(1640,775,0);
INSERT INTO "objlink" VALUES(775,779,12);
INSERT INTO "objlink" VALUES(779,775,0);
INSERT INTO "objlink" VALUES(779,780,9);
INSERT INTO "objlink" VALUES(780,779,12);
INSERT INTO "objlink" VALUES(785,775,2);
INSERT INTO "objlink" VALUES(775,785,0);
CREATE TABLE object (id int, name varchar(40), adam char(1), num int, otype varchar(10), parent int);
INSERT INTO "object" VALUES(339,'Under the Pit','N',1,'Location',0);
INSERT INTO "object" VALUES(775,'Beach','N',1,'Location',0);
INSERT INTO "object" VALUES(779,'The Pit','N',1,'Location',0);
INSERT INTO "object" VALUES(780,'Limbo','N',1,'Location',0);
INSERT INTO "object" VALUES(785,'Foot of Cliff','N',1,'Location',0);
INSERT INTO "object" VALUES(1068,'coracle','I',1,'Item',779);
INSERT INTO "object" VALUES(1522,'ogre','V',1,'Monster',785);
INSERT INTO "object" VALUES(1640,'Coastal Waters','N',1,'Location',0);
INSERT INTO "object" VALUES(1783,'wormhole','D',1,'Door',0);
CREATE TABLE linkobj (src int, dst int, obj int, dir int);
INSERT INTO "linkobj" VALUES(780,779,1783,11);
INSERT INTO "linkobj" VALUES(780,779,1783,8);
INSERT INTO "linkobj" VALUES(779,780,1783,9);
INSERT INTO "linkobj" VALUES(780,779,1783,12);
CREATE TABLE description (id int, desc varchar(500));
INSERT INTO "description" VALUES(339,'You find yourself in a dark and secret place - a huge recess from which there
is no natural exit.  Above you there are some horrendous gurgling noises and
you realise why this place is heavily guarded ...');
INSERT INTO "description" VALUES(775,'This is the middle of the beach.  The sand is golden and clean, and the
driftwood has been arranged into tidy rows.  To the north is the foot of a 
cliff, and to the south you can see the pit with its street lamp.');
INSERT INTO "description" VALUES(779,'You are at the brink of the bubbling pit of Tumbolia.  I wonder what that 
could be for?  The beach lies to the north of here, and beyond that a cliff.
A wormhole at your feet leads, oddly enough, into the sky.  An old-fashioned
street lamp nearby casts a wan glow.');
INSERT INTO "description" VALUES(780,'You are in a semi-infinite expanse of cold blue sky.  Everything you say
echoes around you.  For the main island, you could go up through the wormhole.
For something more irrelevant, go down.  For some quick points, go north.
To the south is a large room full of notice boards.');
INSERT INTO "description" VALUES(785,'You are at the bottom of a sandy cliff north of the beach, and to the west 
you can see a shop of some kind.  The cliff looks just about scalable,
but from the area above you hear terrible roars and cries that sound like
lions and tigers dismembering bears, oh my!');
INSERT INTO "description" VALUES(1640,'You''re at sea, just to the south of the pit, with its plume of toxic waste.');
INSERT INTO "description" VALUES(1068,'A rather nifty little hemispherical boat.');
CREATE TABLE aperture (id int, state varchar(6), trapped bool, accessor int);
INSERT INTO "aperture" VALUES(1783,'shut','f',NULL);
CREATE TABLE objflag (id int, flag varchar(30));
CREATE TABLE objstat (id int, statidx int, statval int);
INSERT INTO "objstat" VALUES(1068,0,50);
INSERT INTO "objstat" VALUES(1068,1,0);
INSERT INTO "objstat" VALUES(1068,2,100);
INSERT INTO "objstat" VALUES(1068,0,50);
INSERT INTO "objstat" VALUES(1068,1,0);
INSERT INTO "objstat" VALUES(1068,2,100);
INSERT INTO "objstat" VALUES(1068,0,50);
INSERT INTO "objstat" VALUES(1068,1,0);
INSERT INTO "objstat" VALUES(1068,2,100);
INSERT INTO "objstat" VALUES(1068,0,50);
INSERT INTO "objstat" VALUES(1068,1,0);
INSERT INTO "objstat" VALUES(1068,2,100);
CREATE TABLE objattr (id int, attrname varchar(30), attrvalue varchar(100));
INSERT INTO "objattr" VALUES(339,'loc_code','AEI');
INSERT INTO "objattr" VALUES(339,'terrain','-');
INSERT INTO "objattr" VALUES(775,'loc_code','AJL');
INSERT INTO "objattr" VALUES(775,'terrain','-');
INSERT INTO "objattr" VALUES(779,'loc_code','AJA');
INSERT INTO "objattr" VALUES(779,'terrain','-');
INSERT INTO "objattr" VALUES(780,'loc_code','AJB');
INSERT INTO "objattr" VALUES(780,'terrain','-');
INSERT INTO "objattr" VALUES(785,'loc_code','AJG');
INSERT INTO "objattr" VALUES(785,'terrain','2');
INSERT INTO "objattr" VALUES(1640,'loc_code','A0M');
INSERT INTO "objattr" VALUES(1640,'terrain','6');
INSERT INTO "objattr" VALUES(1522,'gender','M');
INSERT INTO "objattr" VALUES(1068,'vehicle','0');
CREATE TABLE linkreqobj (src int, dst int, obj int);
COMMIT;
