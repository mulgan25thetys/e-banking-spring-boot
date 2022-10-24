package bank.online.repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import bank.online.entities.ERole;
import bank.online.entities.Role;

@Repository
public interface RoleRepository extends JpaRepository<Role, Integer>{

	Optional<Role> findByName(ERole name);
	
	@Query(value = "SELECT *FROM Role WHERE name =:rolename ",nativeQuery = true)
	public Role getRoleByName(@Param("rolename") String name);
	
	@Query(value = "SELECT * FROM role WHERE name NOT IN "
			+ "('ROLE_CLIENT','ROLE_DECIDEUR','ROLE_MEMBRE_DIRECTOIRE');",nativeQuery = true)
	List<Role> findAllRoleForRH();
	
	@Query(value = "SELECT * FROM role WHERE name IN "
			+ "('ROLE_PERSONNEL_RH','ROLE_DECIDEUR','ROLE_MEMBRE_DIRECTOIRE');",nativeQuery = true)
	List<Role> findAllRoleForDirectoir();
	
	Boolean existsByName(String name);
	
}
