ALTER TABLE fleet ALTER COLUMN id SET DATA TYPE BIGINT;
ALTER TABLE fleet ALTER COLUMN commander SET DATA TYPE BIGINT;
ALTER TABLE member ALTER COLUMN id SET DATA TYPE BIGINT;
ALTER TABLE member ALTER COLUMN fleet_id SET DATA TYPE BIGINT;
