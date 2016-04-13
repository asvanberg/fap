CREATE TABLE fleet (
  id INT NOT NULL,
  name VARCHAR(255) NOT NULL,
  commander INT NOT NULL,
  logged TIMESTAMP NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE member (
  fleet_id INT NOT NULL,
  id INT NOT NULL,
  solar_system VARCHAR(255) NOT NULL,
  ship VARCHAR(255) NOT NULL,
  PRIMARY KEY (fleet_id, id),
  FOREIGN KEY (fleet_id) REFERENCES fleet (id)
);
