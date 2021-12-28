sqlite3 ./DB/groups.db < ./DB/create_db.sql
stack build && stack exec batman-hs-exe
