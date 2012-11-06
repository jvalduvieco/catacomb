CREATE DATABASE catacomb;
CREATE USER 'catacomb'@'localhost' IDENTIFIED BY  'pass';
GRANT USAGE ON * . * TO  'catacomb'@'localhost' IDENTIFIED BY  'pass' ;
GRANT ALL PRIVILEGES ON  'catacomb' . * TO  'catacomb'@'localhost' WITH GRANT OPTION ;

USE catacomb;

DROP TABLE IF EXISTS `character`;
CREATE TABLE IF NOT EXISTS `character` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `user_id` int(11) DEFAULT NULL,
  `name` varchar(50) DEFAULT NULL,
  `max_life_points` int(11) NOT NULL,
  `life_points` int(11) NOT NULL,
  `level` int(11) NOT NULL,
  `experience_points` int(11) NOT NULL,
  `coord_x` int(11) NOT NULL,
  `coord_y` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`),
  KEY `user_id` (`user_id`)
) ENGINE=InnoDB;
INSERT INTO `character` (`id`, `user_id`, `name`, `max_life_points`,
`life_points`, `level`, `experience_points`, `coord_x`, `coord_y`)
VALUES
(1, 1, 'lazi', 232323, 2333, 56, 123098, 5, 5),
(2, 1, 'iguana', 22222, 11, 33, 121211, 6, 6),
(3, 2, 'Sr.Muerte', 3333333, 333, 6, 1238, 7, 7),
(4, 2, 'Ged', 2333233, 1111, 60, 12000, 3, 3),
(5, 3, 'TITO', 33333, 4, 1, 123, 5, 5);
DROP TABLE IF EXISTS `user`;
CREATE TABLE IF NOT EXISTS `user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(50) DEFAULT NULL,
  `password` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB;
INSERT INTO `user` (`id`, `login`, `password`) VALUES
(1, 'jordi', '1a1dc91c907325c69271ddf0c944bc72'), #pass
(2, 'joan', '1a1dc91c907325c69271ddf0c944bc72'),  #pass
(3, 'dani', '1a1dc91c907325c69271ddf0c944bc72');  #pass
ALTER TABLE `character`
ADD CONSTRAINT `character_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES
`user` (`id`);

# MIGRATIONS
# 1 ====
ALTER TABLE `character` ADD public_id int(11) NOT NULL;
UPDATE `character` SET public_id=1001 WHERE id=1;
UPDATE `character` SET public_id=1002 WHERE id=2;
UPDATE `character` SET public_id=1003 WHERE id=3;
UPDATE `character` SET public_id=1004 WHERE id=4;
UPDATE `character` SET public_id=1005 WHERE id=5;
CREATE UNIQUE INDEX character_public_id_idx
ON `character` ( public_id );
