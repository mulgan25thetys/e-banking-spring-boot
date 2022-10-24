package bank.online.entities;

import java.io.Serializable;

import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

import org.hibernate.annotations.Proxy;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.FieldDefaults;

@Entity
@Proxy(lazy=false)
@Getter
@Setter
@FieldDefaults(level = AccessLevel.PRIVATE)
@ToString
@Builder
@AllArgsConstructor
public class User implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	@NotBlank
	private String username;
	private String firstname;
	private String lastname;
	@NotBlank
	@Size(max = 50)
	@Email
	private String email;
	@Size(max = 120)
	private String password;
	private String profile;
	private Integer phone;
	private String country;
	private String city;
	private String address;
	@Temporal(TemporalType.DATE)
	private Date dateCreation;
	@Temporal(TemporalType.DATE)
	private Date dateModification;
	private Boolean status;
	private Integer code;
	@Temporal(TemporalType.DATE)
	private Date codeExpiryDate;
	private Boolean isBanned;
	@Temporal(TemporalType.DATE)
	private Date bannedAt;
	private String profession;
	private Boolean estDisponible;
	private String addressIp;
	@Temporal(TemporalType.TIMESTAMP)
	private Date derniereConnection;
	
	@ManyToOne 
	private IntervalleAge intervalle;
	
	@Enumerated(EnumType.STRING)
	private Gender gender;
	@Enumerated(EnumType.STRING)
	private Civilite civilite;
	@ManyToOne
	private Role role;
	
	@OneToMany(fetch = FetchType.LAZY,cascade = CascadeType.ALL)
	private List<CarteBancaire> carteBancaires;
	
	@OneToOne(cascade = CascadeType.ALL,mappedBy = "emprunteur")
	private Credit credit;
	
	public User() {
	}
	public User(String username, String email, String password,String lastname,String firstname) {
		this.username = username;
		this.email = email;
		this.password = password;
		this.lastname = lastname;
		this.firstname = firstname;
	}
	
	public User(String username, String email, String password,String lastname,String firstname,String region,Integer contact) {
		this.username = username;
		this.email = email;
		this.password = password;
		this.lastname = lastname;
		this.firstname = firstname;
		this.city = region;
		this.phone = contact;
	}
}
