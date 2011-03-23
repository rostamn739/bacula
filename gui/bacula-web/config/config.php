<?php

 /* Language
 en_EN -> English
 es_ES -> Spanish, Mantained by Juan Luis Franc�s Jim�nez.
 it_IT -> Italian, Mantained by Gian Domenico Messina (gianni.messina AT c-ict.it).
 fr_FR -> Frech, Mantained by Morgan LEFIEUX (comete AT daknet.org).
 de_DE -> German, Mantained by Florian Heigl.
 */
 
 $config['language'] = 'en_EN';

 //MySQL bacula catalog
 $config[0]['label'] = 'Backup Server';
 $config[0]['host'] = 'localhost';
 $config[0]['login'] = 'root';
 $config[0]['password'] = 'p@ssw0rd';
 $config[0]['db_name'] = 'bacula';
 $config[0]['db_type'] = 'mysql';
 $config[0]['db_port'] = '3306';
 
  //MySQL bacula catalog
 $config[1]['label'] = 'Backup Server';
 $config[1]['host'] = 'localhost';
 $config[1]['login'] = 'bacula';
 $config[1]['password'] = 'bacula';
 $config[1]['db_name'] = 'bacula';
 $config[1]['db_type'] = 'pgsql';
 $config[1]['db_port'] = '5432';
 
 /* Catalog(s) connection parameters
 Just copy/paste and modify regarding your configuration

 Example: 
 
 //MySQL bacula catalog
 $config[0]['label'] = 'Backup Server';
 $config[0]['host'] = 'localhost';
 $config[0]['login'] = 'bacula';
 $config[0]['password'] = 'verystrongpassword';
 $config[0]['db_name'] = 'bacula';
 $config[0]['db_type'] = 'mysql';
 $config[0]['db_port'] = '3306';

 // PostgreSQL bacula catalog
 $config[0]['label'] = 'Prod Server';
 $config[0]['host'] = 'db-server.domain.com';
 $config[0]['login'] = 'bacula';
 $config[0]['password'] = 'otherstrongpassword';
 $config[0]['db_name'] = 'bacula';
 $config[0]['db_type'] = 'pgsql';
 $config[0]['db_port'] = '5432'; 
 
 // SQLite bacula catalog
 $config[0]['label'] = 'Dev backup server';
 $config[0]['db_path'] = '/path/to/database/db.sdb';

 If you want to configure more than one catalog in Bacula-Web, just copy the first parameters and increment by one the index

  //MySQL bacula catalog
 $config[1]['label'] = 'Dev backup server';
 $config[1]['host'] = 'mysql-server.domain.net';
 $config[1]['login'] = 'bacula';
 $config[1]['password'] = 'verystrongpassword';
 $config[1]['db_name'] = 'bacula';
 $config[1]['db_type'] = 'mysql';
 $config[1]['db_port'] = '3306';
 */
?>
