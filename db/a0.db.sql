PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE player (name varchar(30) not null, password varchar(30) not null);
COMMIT;
