package bank.online.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import bank.online.entities.ERole;
import bank.online.entities.Role;

@Repository
public interface RoleRepository extends JpaRepository<Role, Long>{

	Optional<Role> findByName(ERole name);
	
	@Query(value = "SELECT *FROM Role WHERE name =:rolename ",nativeQuery = true)
	public Role getRoleByName(@Param("rolename") String name);
	
}
